package com.fackbook.Share.Service;

import com.fackbook.Post.Service.PostService;
import com.fackbook.Share.DTO.ShareDTO;
import com.fackbook.Share.Entity.Share;
import com.fackbook.Share.Mapper.ShareMapper;
import com.fackbook.Share.Repository.ShareRepository;
import com.fackbook.User.Service.UserService;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.math.BigInteger;
import java.time.LocalDateTime;

@Service
@RequiredArgsConstructor
public class ShareService {
    private final ShareRepository shareRepository;
    private final PostService postService;
    private final UserService userService;
    public ShareDTO getShareById(Long id){
        return ShareMapper.toDTO(
                shareRepository.findById(id).orElseThrow(()->new
                        IllegalArgumentException("This post not shared"))
        );
    }
    public Page<ShareDTO> getSharesByUserId(Long userId, Pageable pageable){
        return shareRepository.findByUser_Id(userId,pageable).map(ShareMapper::toDTO);
    }
    public Page<ShareDTO> getSharesByPostId(Long postId,Pageable pageable){
        return shareRepository.findByPost_Id(postId,pageable).map(ShareMapper::toDTO);
    }
    @Transactional
    public ShareDTO createNewShare(Long userId,Long postId,ShareDTO dto){
        var user = userService.getUserEntityById(userId);
        var post =postService.getPostEntityById(postId);

        Share share = Share.builder()
                .user(user)
                .post(post)
                .content(dto.getContent())
                .createdAt(LocalDateTime.now())
                .build();
        var savedShare = shareRepository.save(share);
        post.getShares().add(share);
        post.setNumberOfShares(post.getNumberOfShares().add(BigInteger.ONE));
        postService.savePost(post);
        return ShareMapper.toDTO(savedShare);
    }
    public Page<ShareDTO> getAllShares(Pageable pageable){
        return shareRepository.findAll(pageable).map(ShareMapper::toDTO);
    }
}
