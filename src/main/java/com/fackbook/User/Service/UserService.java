package com.fackbook.User.Service;

import com.fackbook.Exception.ImageUploadException;
import com.fackbook.Shared.Image.UploadImageService;
import com.fackbook.User.DTO.UserDTO;
import com.fackbook.User.Entity.User;
import com.fackbook.User.Enum.Gender;
import com.fackbook.User.Enum.Role;
import com.fackbook.User.Enum.Status;
import com.fackbook.User.Mapper.UserMapper;
import com.fackbook.User.Repository.UserRepository;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.cache.annotation.Caching;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.time.LocalDateTime;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class UserService {
    private final UserRepository userRepository;
    private final UploadImageService uploadImageService;

    public Boolean userExistsByEmail(String email){
        return  userRepository.findByEmail(email).isPresent();
    }
    public Boolean userExistsByPhone(String phone){
        return userRepository.findByPhone(phone).isPresent();
    }
    @Cacheable(value = "usersById",key = "#id")
    public UserDTO getUserById(Long id){
        return UserMapper.toDTO(
                userRepository.findById(id).orElseThrow(()
                -> new IllegalArgumentException("User Not Found !")));
    }
    public User getUserEntityById(Long id){
        return userRepository.findById(id).orElseThrow(()
                -> new IllegalArgumentException("User Not Found !"));
    }
    public void updateLastLoginDateById(Long id){
        var user = getUserEntityById(id);
        user.setLastLoginDate(LocalDateTime.now());
        userRepository.save(user);
    }
    public User getUserEntityByEmail(String email){
        return userRepository.findByEmail(email).orElseThrow(()
                -> new IllegalArgumentException("User or Email are Not Found !"));
    }
    @Cacheable(value = "usersByEmail",key = "#email")
    public UserDTO getUserByEmail(String email){
        return UserMapper.toDTO(
                userRepository.findByEmail(email).orElseThrow(()
                        -> new IllegalArgumentException("User or Email are Not Found !")));
    }
    @Cacheable(value = "usersByPhone",key = "#phone")
    public UserDTO getUserByPhone(String phone){
        return UserMapper.toDTO(
                userRepository.findByPhone(phone).orElseThrow(()
                        -> new IllegalArgumentException("User or Phone Number are Not Found !")));
    }
    private Page<UserDTO> getUsersByGender(Gender gender, Pageable pageable){
        return userRepository.findByGender(gender,pageable).map(UserMapper::toDTO);
    }
    private Page<UserDTO> getUsersByRole(Role role, Pageable pageable){
        return userRepository.findByRole(role,pageable).map(UserMapper::toDTO);
    }
    private Page<UserDTO> getUsersByStatus(Status status, Pageable pageable){
        return userRepository.findByStatus(status,pageable).map(UserMapper::toDTO);
    }
    public Page<UserDTO> getMaleUsers(Pageable pageable){
        return getUsersByGender(Gender.MALE,pageable);
    }
    public Page<UserDTO> getFemaleUsers(Pageable pageable){
        return getUsersByGender(Gender.FEMALE,pageable);
    }
    public Page<UserDTO> getSystemAdminUsers(Pageable pageable){
        return getUsersByRole(Role.SYSTEM_ADMIN,pageable);
    }
    public Page<UserDTO> getGroupAdminUsers(Pageable pageable){
        return getUsersByRole(Role.GROUP_ADMIN,pageable);
    }
    public Page<UserDTO> getGroupMemberUsers(Pageable pageable){
        return getUsersByRole(Role.GROUP_MEMBER,pageable);
    }
    public Page<UserDTO> getNormalUsers(Pageable pageable){
        return getUsersByRole(Role.USER,pageable);
    }
    public Page<UserDTO> getActiveUsers(Pageable pageable){
        return getUsersByStatus(Status.ACTIVE,pageable);
    }
    public Page<UserDTO> getInactiveUsers(Pageable pageable){
        return getUsersByStatus(Status.INACTIVE,pageable);
    }
    public Page<UserDTO> getBannedUsers(Pageable pageable){
        return getUsersByStatus(Status.BANNED,pageable);
    }
    public Page<UserDTO> getDeletedUsers(Pageable pageable){
        return getUsersByStatus(Status.DELETED,pageable);
    }
    public Page<UserDTO> getUsersByName(String name,Pageable pageable){
        return userRepository.findByNameContainingIgnoreCase(name,pageable).map(UserMapper::toDTO);
    }
    public Page<UserDTO> getUsersByAddress(String address,Pageable pageable){
        return userRepository.findByAddressContainingIgnoreCase(address,pageable).map(UserMapper::toDTO);
    }
    @Transactional
    private UserDTO changeUserStatus(Long id,Status status,Boolean deleted,Boolean enabled){
        var user = getUserEntityById(id);
        Optional.ofNullable(status).ifPresent(user::setStatus);
        Optional.ofNullable(deleted).ifPresent(user::setDeleted);
        Optional.ofNullable(enabled).ifPresent(user::setEnabled);
        if (Boolean.TRUE.equals(deleted)) {
            user.setDeletedAt(LocalDateTime.now());
        } else {
            user.setDeletedAt(null);
        }
        user.setUpdatedAt(LocalDateTime.now());
        return UserMapper.toDTO(userRepository.save(user));
    }
    @Transactional
    private UserDTO changeUserRole(Long id,Role role){
        var user = getUserEntityById(id);
        Optional.ofNullable(role).ifPresent(user::setRole);
        user.setUpdatedAt(LocalDateTime.now());
        return UserMapper.toDTO(userRepository.save(user));
    }
    @CachePut(value = "usersById",key = "#id")
    public UserDTO toSystemAdmin(Long id){
        return changeUserRole(id,Role.SYSTEM_ADMIN);
    }
    @CachePut(value = "usersById",key = "#id")
    public UserDTO toGroupAdmin(Long id){
        return changeUserRole(id,Role.GROUP_ADMIN);
    }
    @CachePut(value = "usersById",key = "#id")
    public UserDTO toGroupMember(Long id){
        return changeUserRole(id,Role.GROUP_MEMBER);
    }
    @CachePut(value = "usersById",key = "#id")
    public UserDTO toNormalUser(Long id){
        return changeUserRole(id,Role.USER);
    }
    @CachePut(value = "usersById",key = "#id")
    public UserDTO activeUser(Long id){
        return changeUserStatus(id,Status.ACTIVE,false,true);
    }
    @CachePut(value = "usersById",key = "#id")
    public UserDTO inactiveUser(Long id){
        return changeUserStatus(id,Status.INACTIVE,false,true);
    }
    @CachePut(value = "usersById",key = "#id")
    public UserDTO banUser(Long id){
        return changeUserStatus(id,Status.BANNED,false,true);
    }
    @CacheEvict(value = "usersById", key = "#id")
    public UserDTO deleteUser(Long id){
        return changeUserStatus(id,Status.DELETED,true,false);
    }
    @Transactional
    @CachePut(value = "usersById",key = "#id")
    public UserDTO updateUser(Long id,UserDTO dto){
        var user = getUserEntityById(id);

        Optional.ofNullable(dto.getName()).ifPresent(user::setName);
        Optional.ofNullable(dto.getAddress()).ifPresent(user::setAddress);
        Optional.ofNullable(dto.getBio()).ifPresent(user::setBio);
        Optional.ofNullable(dto.getBirthdate()).ifPresent(user::setBirthdate);
        Optional.ofNullable(dto.getGender()).ifPresent(user::setGender);

        user.setUpdatedAt(LocalDateTime.now());
        return UserMapper.toDTO(userRepository.save(user));
    }
    @Transactional
    @CachePut(value = "usersById",key = "#id")
    public UserDTO updateProfileImage(Long id,MultipartFile image){
        var user = getUserEntityById(id);
        if(image != null && !image.isEmpty()){
            try{
                user.setImageUrl(uploadImageService.uploadMultipartFile(image));
            }
            catch (Exception e){
                throw new ImageUploadException("The Image Uploading Process Failed !",e.getCause());
            }
        }
        user.setUpdatedAt(LocalDateTime.now());
        return UserMapper.toDTO(userRepository.save(user));
    }
}
