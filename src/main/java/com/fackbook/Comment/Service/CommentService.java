package com.fackbook.Comment.Service;

import com.fackbook.Comment.DTO.CommentDTO;
import com.fackbook.Comment.Entity.Comment;
import com.fackbook.Comment.Mapper.CommentMapper;
import com.fackbook.Comment.Repository.CommentRepository;
import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Enum.VisibilityStatus;
import com.fackbook.Post.Util.Service.AccessibilityService;
import com.fackbook.Post.Service.PostService;
import com.fackbook.Post.Util.Service.MediaManager;
import com.fackbook.Shared.Helper.FileHelper;
import com.fackbook.User.Service.UserService;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.Optional;
@Service
@RequiredArgsConstructor
public class CommentService {
    private final CommentRepository commentRepository;
    private final UserService userService;
    private final PostService postService;
    private final FileHelper fileHelper;
    private final AccessibilityService accessibilityService;
    private final MediaManager mediaManager;

    public Comment getCommentEntityById(Long commentId){
       return commentRepository.findById(commentId).orElseThrow(()->
               new IllegalArgumentException("Comment Not Found !"));
    }

    @Cacheable(value = "commentsByUserAndCommentIDs",key = "#userId + '-' + #commentId")
    public CommentDTO getCommentById(Long userId,Long commentId){
        var comment = getCommentEntityById(commentId);
        accessibilityService.validateVisibility(comment,userId);
        accessibilityService.validateModeration(comment,userId);
        return CommentMapper.toDTO(comment);
    }
    public Page<CommentDTO> getCommentsByPostId(Long userId,Long postId, Pageable pageable){
        var comments = commentRepository.findByPost_Id(postId,pageable);
        return comments.map(
                comment -> {
                    accessibilityService.validateVisibility(comment,userId);
                    accessibilityService.validateModeration(comment,userId);
                    return CommentMapper.toDTO(comment);
                }
        );
    }
    public Page<CommentDTO> getCommentsByUserName(Long userId,String userName, Pageable pageable){
        var comments = commentRepository.findByUser_NameContainingIgnoreCase(userName,pageable);
        return comments.map(
                comment -> {
                    accessibilityService.validateVisibility(comment,userId);
                    accessibilityService.validateModeration(comment,userId);
                    return CommentMapper.toDTO(comment);
                }
        );
    }
    public Page<CommentDTO> getCommentsByUserId(Long userId,Pageable pageable){
        var comments = commentRepository.findByUser_Id(userId,pageable);
        return comments.map(
                comment -> {
                    accessibilityService.validateVisibility(comment,userId);
                    accessibilityService.validateModeration(comment,userId);
                    return CommentMapper.toDTO(comment);
                }
        );
    }
    private Page<CommentDTO> getCommentsByModerationStatus(Long userId,ModerationStatus moderationStatus,Pageable pageable){
        return commentRepository.findByModerationStatus(moderationStatus,pageable).map(
                comment -> {
                    accessibilityService.validateVisibility(comment,userId);
                    accessibilityService.validateModeration(comment,userId);
                    return CommentMapper.toDTO(comment);
                }
                );
    }
    private Page<CommentDTO> getCommentsByVisibilityStatus(Long userId,VisibilityStatus visibilityStatus,Pageable pageable){
        return commentRepository.findByVisibilityStatus(visibilityStatus,pageable).map(
                comment -> {
                    accessibilityService.validateVisibility(comment,userId);
                    accessibilityService.validateModeration(comment,userId);
                    return CommentMapper.toDTO(comment);
                }
        );
    }
    public Page<CommentDTO> getActiveComments(Long userId,Pageable pageable){
        return getCommentsByVisibilityStatus(userId,VisibilityStatus.ACTIVE,pageable);
    }
    public Page<CommentDTO> getHiddenComments(Long userId,Pageable pageable){
        return getCommentsByVisibilityStatus(userId,VisibilityStatus.HIDDEN,pageable);
    }
    public Page<CommentDTO> getDeletedComments(Long userId,Pageable pageable){
        return getCommentsByVisibilityStatus(userId,VisibilityStatus.DELETED,pageable);
    }
    public Page<CommentDTO> getRemovedByAdminComments(Long userId,Pageable pageable){
        return getCommentsByVisibilityStatus(userId,VisibilityStatus.REMOVED_BY_ADMIN,pageable);
    }
    public Page<CommentDTO> getReportedComments(Long userId,Pageable pageable){
        return getCommentsByModerationStatus(userId,ModerationStatus.REPORTED,pageable);
    }
    public Page<CommentDTO> getRejectedComments(Long userId,Pageable pageable){
        return getCommentsByModerationStatus(userId,ModerationStatus.REJECTED,pageable);
    }
    public Page<CommentDTO> getUnderReviewComments(Long userId,Pageable pageable){
        return getCommentsByModerationStatus(userId,ModerationStatus.UNDER_REVIEW,pageable);
    }
    public Page<CommentDTO> getPendingApprovalComments(Long userId,Pageable pageable){
        return getCommentsByModerationStatus(userId,ModerationStatus.PENDING_APPROVAL,pageable);
    }
    @Cacheable(value = "commentsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public CommentDTO getCommentByUserIdAndPostId(Long userId,Long postId){
        var comment = commentRepository.findByUser_IdAndPost_Id(userId,postId).orElseThrow(
                    () ->new IllegalArgumentException("Comment Not Found !"));
        accessibilityService.validateVisibility(comment,userId);
        accessibilityService.validateModeration(comment,userId);
        return CommentMapper.toDTO(comment);
    }
    @Transactional
    public CommentDTO createNewComment(Long userId, Long postId, CommentDTO dto, MultipartFile image,MultipartFile video){
        var user = userService.getUserEntityById(userId);
        var post = postService.getPostEntityById(postId);
        var imageUrl = fileHelper.generateImageUrl(image);
        var videoUrl = fileHelper.generateVideoUrl(video,"Uploaded By "+user.getName()+" @"+user.getId(),dto.getContent());
        boolean isContentNull = dto.getContent() == null || dto.getContent().isBlank();
        boolean isImageNull = imageUrl == null || imageUrl.isBlank();
        boolean isVideoNull = videoUrl == null || videoUrl.isBlank();
        if(isContentNull && isImageNull && isVideoNull){
            throw new IllegalArgumentException("Comment Couldn't be empty !!");
        }
        Comment comment = Comment.builder()
                .content(dto.getContent())
                .imageUrl(imageUrl)
                .videoUrl(videoUrl)
                .user(user)
                .post(post)
                .createdAt(LocalDateTime.now())
                .deleted(false)
                .deletedAt(null)
                .numberOfReplies(BigInteger.ZERO)
                .numberOfReacts(BigInteger.ZERO)
                .moderationStatus(post.getGroup() != null ? ModerationStatus.PENDING_APPROVAL:ModerationStatus.NONE)
                .visibilityStatus(dto.getVisibilityStatus() != null ? dto.getVisibilityStatus() : VisibilityStatus.ACTIVE)
                .build();
        commentRepository.save(comment);
        post.getComments().add(comment);
        if(comment.getModerationStatus().equals(ModerationStatus.NONE) && comment.getVisibilityStatus().equals(VisibilityStatus.ACTIVE)){
            post.setNumberOfComments(post.getNumberOfComments().add(BigInteger.ONE));
        }
        postService.savePost(post);
        return CommentMapper.toDTO(comment);
    }
    @Transactional
    @CachePut(value = "commentsByUserAndCommentIDs",key = "#userId + '-' + #commentId")
    public CommentDTO updateComment(Long userId,Long commentId,CommentDTO dto,MultipartFile image,MultipartFile video){
        var comment = getCommentEntityById(commentId);
        accessibilityService.validateVisibility(comment,userId);
        accessibilityService.validateModeration(comment,userId);
        var user =userService.getUserEntityById(userId);
        var post = comment.getPost();
        mediaManager.handleMedia(comment,image,video,dto.getRemoveImage(),dto.getRemoveVideo());
        var imageUrl = comment.getImageUrl();
        var videoUrl = comment.getVideoUrl();
        boolean isContentNull = dto.getContent() == null || dto.getContent().isBlank();
        boolean isImageNull = imageUrl == null || imageUrl.isBlank();
        boolean isVideoNull = videoUrl == null || videoUrl.isBlank();
        if(isContentNull && isImageNull && isVideoNull){
            throw new IllegalArgumentException("Comment Couldn't be empty !!");
        }
        Optional.ofNullable(dto.getContent()).ifPresent(comment::setContent);
        comment.setUpdatedAt(LocalDateTime.now());
        VisibilityStatus newVisibilityStatus = dto.getVisibilityStatus() != null ? dto.getVisibilityStatus() : VisibilityStatus.ACTIVE;
        ModerationStatus newModerationStatus = post.getGroup() != null ? ModerationStatus.PENDING_APPROVAL : ModerationStatus.NONE;

        boolean wasCounted = comment.getModerationStatus() == ModerationStatus.NONE
                && comment.getVisibilityStatus() == VisibilityStatus.ACTIVE;
        boolean willBeCounted = newModerationStatus == ModerationStatus.NONE
                && newVisibilityStatus == VisibilityStatus.ACTIVE;

        comment.setVisibilityStatus(newVisibilityStatus);
        comment.setModerationStatus(newModerationStatus);

        BigInteger delta = BigInteger.valueOf((willBeCounted ? 1 : 0) - (wasCounted ? 1 : 0));
        post.setNumberOfComments(post.getNumberOfComments().add(delta));
        postService.savePost(post);
        return CommentMapper.toDTO(comment);
    }
    @Transactional
    private CommentDTO changeCommentStatus(Long userId,Long commentId,BigInteger postCounterChange,VisibilityStatus visibilityStatus,ModerationStatus moderationStatus,Boolean deleted){
        var comment = getCommentEntityById(commentId);
        accessibilityService.validateVisibility(comment,userId);
        accessibilityService.validateModeration(comment,userId);
        var user = userService.getUserEntityById(userId);
        var post = postService.getPostEntityById(comment.getPost().getId());
        Optional.ofNullable(visibilityStatus).ifPresent(comment::setVisibilityStatus);
        Optional.ofNullable(moderationStatus).ifPresent(comment::setModerationStatus);
        Optional.ofNullable(deleted).ifPresent(comment::setDeleted);
        if(Boolean.TRUE.equals(deleted)){
            comment.setDeletedAt(LocalDateTime.now());
        }
        if(Boolean.FALSE.equals(deleted)){
            comment.setDeletedAt(null);
        }
        comment.setUpdatedAt(LocalDateTime.now());
        post.setNumberOfComments(post.getNumberOfComments().add(postCounterChange));
        postService.savePost(post);
        return CommentMapper.toDTO(commentRepository.save(comment));
    }
    @CachePut(value = "commentsByUserAndCommentIDs",key = "#userId + '-' + #commentId")
    public CommentDTO activeComment(Long userId,Long commentId){
        return changeCommentStatus(userId,commentId,BigInteger.ONE,VisibilityStatus.ACTIVE,ModerationStatus.NONE,false);
    }
    @CachePut(value = "commentsByUserAndCommentIDs",key = "#userId + '-' + #commentId")
    public CommentDTO hideComment(Long userId,Long commentId){
        return changeCommentStatus(userId,commentId,BigInteger.valueOf(-1),VisibilityStatus.HIDDEN,null,false);
    }
    @CacheEvict(value = "commentsByUserAndCommentIDs",key = "#userId + '-' + #commentId")
    public CommentDTO removeByAdminComment(Long userId,Long commentId){
        return changeCommentStatus(userId,commentId,BigInteger.valueOf(-1),VisibilityStatus.REMOVED_BY_ADMIN,null,true);
    }
    @CacheEvict(value = "commentsByUserAndCommentIDs",key = "#userId + '-' + #commentId")
    public CommentDTO deleteComment(Long userId,Long commentId){
        return changeCommentStatus(userId,commentId,BigInteger.valueOf(-1),VisibilityStatus.DELETED,ModerationStatus.NONE,true);
    }
    @CachePut(value = "commentsByUserAndCommentIDs",key = "#userId + '-' + #commentId")
    public CommentDTO setCommentAsReported(Long userId,Long commentId){
        return changeCommentStatus(userId,commentId,BigInteger.valueOf(-1),VisibilityStatus.HIDDEN,ModerationStatus.REPORTED,false);
    }
    @CachePut(value = "commentsByUserAndCommentIDs",key = "#userId + '-' + #commentId")
    public CommentDTO setCommentAsUnderReview(Long userId,Long commentId){
        return changeCommentStatus(userId,commentId,BigInteger.valueOf(-1),VisibilityStatus.HIDDEN,ModerationStatus.UNDER_REVIEW,false);
    }
    @CachePut(value = "commentsByUserAndCommentIDs",key = "#userId + '-' + #commentId")
    public CommentDTO setCommentAsPendingApproval(Long userId,Long commentId){
        return changeCommentStatus(userId,commentId,BigInteger.valueOf(-1),VisibilityStatus.HIDDEN,ModerationStatus.PENDING_APPROVAL,false);
    }
    @CacheEvict(value = "commentsByUserAndCommentIDs",key = "#userId + '-' + #commentId")
    public CommentDTO setCommentAsRejected(Long userId,Long commentId){
        return changeCommentStatus(userId,commentId,BigInteger.valueOf(-1),VisibilityStatus.REMOVED_BY_ADMIN,ModerationStatus.REJECTED,true);
    }

    public void saveComment(Comment comment){
        commentRepository.save(comment);
    }
}


