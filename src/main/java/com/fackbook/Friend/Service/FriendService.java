package com.fackbook.Friend.Service;


import com.fackbook.Friend.DTO.FriendshipDTO;
import com.fackbook.Friend.Entity.Friendship;
import com.fackbook.Friend.Enum.Status;
import com.fackbook.Friend.Mapper.FriendshipMapper;
import com.fackbook.Friend.Repository.FriendshipRepository;
import com.fackbook.User.Service.UserService;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class FriendService {
    private final FriendshipRepository friendshipRepository;
    private final UserService userService;

    public Page<FriendshipDTO> getUserFriendshipsByUserId(Long userId, Pageable pageable){
        return friendshipRepository.findByUser_IdAndStatus(userId, Status.FRIENDS,pageable).map(FriendshipMapper::toDTO);
    }

    public Page<FriendshipDTO> getFriendFriendshipsByFriendId(Long friendId,Pageable pageable){
        return friendshipRepository.findByFriend_idAndStatus(friendId,Status.FRIENDS,pageable).map(FriendshipMapper::toDTO);
    }
    private Optional<Friendship> getFriendshipEntityByUserIdAndFriendId(Long userId, Long friendId){
        if(userId.equals(friendId)){
            throw new IllegalArgumentException("User Couldn't be friend with himself !!");
        }
        Long actualUserId = Long.min(userId,friendId);
        Long actualFriendId = Long.max(userId,friendId);
        return friendshipRepository.findByUser_IdAndFriend_Id(actualUserId,actualFriendId);
    }
    @Cacheable(value = "friendshipsByUserAndFriendIDs",
            key = "T(java.lang.Math).min(#userId,#friendId) + '-' + T(java.lang.Math).max(#userId,#friendId)")
    public FriendshipDTO getFriendshipByUserIdAndFriendId(Long userId,Long friendId){
        var friendship = getFriendshipEntityByUserIdAndFriendId(userId,friendId).
                orElseThrow(()->new IllegalArgumentException("Friendship Not Found !"));
        if(friendship.getStatus().equals(Status.NOT_FRIENDS)){
            throw new IllegalArgumentException("This friendship has been ended !");
        }
        return FriendshipMapper.toDTO(friendship);
    }
    @Cacheable(value = "friendshipsByFriendshipId",key = "#friendshipId")
    public FriendshipDTO getFriendshipByFriendshipId(Long friendshipId){
        return FriendshipMapper.toDTO(friendshipRepository.findById(friendshipId)
                .orElseThrow(()->new IllegalArgumentException("Friendship Not Found !")));
    }
    public Page<FriendshipDTO> getAllFriendships(Pageable pageable){
        return friendshipRepository.findByStatus(Status.FRIENDS,pageable).map(FriendshipMapper::toDTO);
    }
    public Page<FriendshipDTO> getAllEndedFriendships(Pageable pageable){
        return friendshipRepository.findByStatus(Status.NOT_FRIENDS,pageable).map(FriendshipMapper::toDTO);
    }

    @Transactional
    @CacheEvict(value = "friendshipsByUserAndFriendIDs",
            key = "T(java.lang.Math).min(#userId,#friendId) + '-' + T(java.lang.Math).max(#userId,#friendId)")
    public FriendshipDTO createFriendship(Long userId,Long friendId){
        var friendship = getFriendshipEntityByUserIdAndFriendId(userId,friendId);
        if(friendship.isEmpty()){
            Friendship newFriendship = Friendship.builder()
                    .user(userService.getUserEntityById(userId))
                    .friend(userService.getUserEntityById(friendId))
                    .createdAt(LocalDateTime.now())
                    .status(Status.FRIENDS)
                    .build();
            return FriendshipMapper.toDTO(friendshipRepository.save(newFriendship));
        }
            Friendship existFriendship = friendship.get();
            if(existFriendship.getStatus().equals(Status.NOT_FRIENDS)){
                existFriendship.setStatus(Status.FRIENDS);
                existFriendship.setDeleted(false);
                existFriendship.setDeletedAt(null);
                existFriendship.setUpdatedAt(LocalDateTime.now());
                return FriendshipMapper.toDTO(friendshipRepository.save(existFriendship));
            }
            else {
                throw new IllegalArgumentException("The Friendship Already Exists !!");
            }
    }
    @Transactional
    @CacheEvict(value = "friendshipsByUserAndFriendIDs",
            key = "T(java.lang.Math).min(#userId,#friendId) + '-' + T(java.lang.Math).max(#userId,#friendId)")
    public FriendshipDTO endFriendshipByUserIdAndFriendId(Long userId,Long friendId){
        var friendship = getFriendshipEntityByUserIdAndFriendId(userId,friendId);
        if(friendship.isEmpty()){
            throw new IllegalArgumentException("Friendship Doesn't Exists !!");
        }
        Friendship existFriendship = friendship.get();
        if(existFriendship.getStatus().equals(Status.FRIENDS)){
            existFriendship.setStatus(Status.NOT_FRIENDS);
            existFriendship.setDeleted(true);
            existFriendship.setDeletedAt(LocalDateTime.now());
            return FriendshipMapper.toDTO(friendshipRepository.save(existFriendship));
        }
        else {
            throw new IllegalArgumentException("Friendship already ended !!");
        }
    }
}
