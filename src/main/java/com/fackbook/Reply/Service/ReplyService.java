package com.fackbook.Reply.Service;

import com.fackbook.Comment.DTO.CommentDTO;
import com.fackbook.Comment.Entity.Comment;
import com.fackbook.Comment.Mapper.CommentMapper;
import com.fackbook.Comment.Service.CommentService;
import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Enum.VisibilityStatus;
import com.fackbook.Post.Util.Service.AccessibilityService;
import com.fackbook.Post.Service.PostService;
import com.fackbook.Post.Util.Service.MediaManager;
import com.fackbook.Reply.DTO.ReplyDTO;
import com.fackbook.Reply.Entity.Reply;
import com.fackbook.Reply.Mapper.ReplyMapper;
import com.fackbook.Reply.Repository.ReplyRepository;
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
public class ReplyService {
    private final CommentService commentService;
    private final PostService postService;
    private final UserService userService;
    private final AccessibilityService accessibilityService;
    private final FileHelper fileHelper;
    private final ReplyRepository replyRepository;
    private final MediaManager mediaManager;

    public Reply getReplyEntityById(Long id){
        return replyRepository.findById(id).orElseThrow(()->new IllegalArgumentException("Reply Not Found !"));
    }
    @Cacheable(value = "repliesByUserAndReplyIDs",key = "#userId + '-' + #replyId")
    public ReplyDTO getReplyById(Long userId,Long replyId){
        var reply = getReplyEntityById(replyId);
        accessibilityService.validateVisibility(reply,userId);
        accessibilityService.validateModeration(reply,userId);
        return ReplyMapper.toDTO(reply);
    }

    public Page<ReplyDTO> getRepliesByCommentId(Long userId, Long commentId, Pageable pageable){
        return replyRepository.findByComment_Id(commentId,pageable).map(
                reply -> {
                    accessibilityService.validateVisibility(reply,userId);
                    accessibilityService.validateModeration(reply,userId);
                    return ReplyMapper.toDTO(reply);
                }
        );
    }
    public Page<ReplyDTO> getRepliesByUserId(Long userId,Pageable pageable){
        return replyRepository.findByUser_Id(userId,pageable).map(
                reply -> {
                    accessibilityService.validateVisibility(reply,userId);
                    accessibilityService.validateModeration(reply,userId);
                    return ReplyMapper.toDTO(reply);
                }
        );
    }
    public Page<ReplyDTO> getRepliesByPostId(Long userId,Long postId,Pageable pageable){
        return replyRepository.findByComment_Post_Id(postId,pageable).map(
                reply -> {
                    accessibilityService.validateVisibility(reply,userId);
                    accessibilityService.validateModeration(reply,userId);
                    return ReplyMapper.toDTO(reply);
                }
        );
    }
    public Page<ReplyDTO> getRepliesByUserName(Long userId,String userName,Pageable pageable){
        return replyRepository.findByUser_NameContainingIgnoreCase(userName,pageable).map(
                reply -> {
                    accessibilityService.validateVisibility(reply,userId);
                    accessibilityService.validateModeration(reply,userId);
                    return ReplyMapper.toDTO(reply);
                }
        );
    }

    private Page<ReplyDTO> getRepliesByVisibilityStatus(Long userId, VisibilityStatus visibilityStatus,Pageable pageable){
        return replyRepository.findByVisibilityStatus(visibilityStatus,pageable).map(
                reply -> {
                    accessibilityService.validateVisibility(reply,userId);
                    accessibilityService.validateModeration(reply,userId);
                    return ReplyMapper.toDTO(reply);
                }
        );
    }
    private Page<ReplyDTO> getRepliesByModerationStatus(Long userId, ModerationStatus moderationStatus, Pageable pageable){
        return replyRepository.findByModerationStatus(moderationStatus,pageable).map(
                reply -> {
                    accessibilityService.validateVisibility(reply,userId);
                    accessibilityService.validateModeration(reply,userId);
                    return ReplyMapper.toDTO(reply);
                }
        );
    }
    public Page<ReplyDTO> getActiveComments(Long userId, Pageable pageable){
        return getRepliesByVisibilityStatus(userId,VisibilityStatus.ACTIVE,pageable);
    }
    public Page<ReplyDTO> getHiddenComments(Long userId,Pageable pageable){
        return getRepliesByVisibilityStatus(userId,VisibilityStatus.HIDDEN,pageable);
    }
    public Page<ReplyDTO> getDeletedComments(Long userId,Pageable pageable){
        return getRepliesByVisibilityStatus(userId,VisibilityStatus.DELETED,pageable);
    }
    public Page<ReplyDTO> getRemovedByAdminComments(Long userId,Pageable pageable){
        return getRepliesByVisibilityStatus(userId,VisibilityStatus.REMOVED_BY_ADMIN,pageable);
    }
    public Page<ReplyDTO> getReportedComments(Long userId,Pageable pageable){
        return getRepliesByModerationStatus(userId,ModerationStatus.REPORTED,pageable);
    }
    public Page<ReplyDTO> getRejectedComments(Long userId,Pageable pageable){
        return getRepliesByModerationStatus(userId,ModerationStatus.REJECTED,pageable);
    }
    public Page<ReplyDTO> getUnderReviewComments(Long userId,Pageable pageable){
        return getRepliesByModerationStatus(userId,ModerationStatus.UNDER_REVIEW,pageable);
    }
    public Page<ReplyDTO> getPendingApprovalComments(Long userId,Pageable pageable){
        return getRepliesByModerationStatus(userId,ModerationStatus.PENDING_APPROVAL,pageable);
    }
    @Transactional
    public ReplyDTO createNewReply(Long userId, Long commentId,ReplyDTO dto ,MultipartFile image , MultipartFile video){
        var user = userService.getUserEntityById(userId);
        var comment = commentService.getCommentEntityById(commentId);
        var imageUrl = fileHelper.generateImageUrl(image);
        var videoUrl = fileHelper.generateVideoUrl(video,"Uploaded By "+user.getName()+" @"+user.getId(),dto.getContent());
        boolean isContentNull = dto.getContent() == null || dto.getContent().isBlank();
        boolean isImageNull = imageUrl == null || imageUrl.isBlank();
        boolean isVideoNull = videoUrl == null || videoUrl.isBlank();
        if(isContentNull && isImageNull && isVideoNull){
            throw new IllegalArgumentException("Reply Couldn't be empty !!");
        }
        Reply reply = Reply.builder()
                .user(user)
                .comment(comment)
                .content(dto.getContent())
                .imageUrl(imageUrl)
                .videoUrl(videoUrl)
                .numberOfReacts(BigInteger.ZERO)
                .createdAt(LocalDateTime.now())
                .moderationStatus(comment.getPost().getGroup() != null ? ModerationStatus.PENDING_APPROVAL:ModerationStatus.NONE)
                .visibilityStatus(dto.getVisibilityStatus() != null ? dto.getVisibilityStatus() : VisibilityStatus.ACTIVE)
                .deleted(false)
                .deletedAt(null)
                .build();
        replyRepository.save(reply);
        comment.getReplies().add(reply);
        comment.setNumberOfReplies(comment.getNumberOfReplies().add(BigInteger.ONE));
        commentService.saveComment(comment);
        return ReplyMapper.toDTO(reply);
    }

    @Transactional
    @CachePut(value = "repliesByUserAndReplyIDs",key = "#userId + '-' + #replyId")
    public ReplyDTO updateReply(Long userId,Long replyId,ReplyDTO dto,MultipartFile image,MultipartFile video){
        var user = userService.getUserEntityById(userId);
        var reply = getReplyEntityById(replyId);
        var comment = commentService.getCommentEntityById(reply.getComment().getId());
        accessibilityService.validateVisibility(reply,userId);
        accessibilityService.validateModeration(reply,userId);
        mediaManager.handleMedia(reply,image,video,dto.getRemoveImage(),dto.getRemoveVideo());
        var imageUrl = reply.getImageUrl();
        var videoUrl = reply.getVideoUrl();
        boolean isContentNull = dto.getContent() == null || dto.getContent().isBlank();
        boolean isImageNull = imageUrl == null || imageUrl.isBlank();
        boolean isVideoNull = videoUrl == null || videoUrl.isBlank();
        if(isContentNull && isImageNull && isVideoNull){
            throw new IllegalArgumentException("Reply Couldn't be empty !!");
        }
        Optional.ofNullable(dto.getContent()).ifPresent(reply::setContent);
        reply.setUpdatedAt(LocalDateTime.now());
        VisibilityStatus newVisibilityStatus = dto.getVisibilityStatus() != null ? dto.getVisibilityStatus() : VisibilityStatus.ACTIVE;
        ModerationStatus newModerationStatus = reply.getComment().getPost().getGroup() != null ? ModerationStatus.PENDING_APPROVAL : ModerationStatus.NONE;

        boolean wasCounted = reply.getModerationStatus() == ModerationStatus.NONE
                && reply.getVisibilityStatus() == VisibilityStatus.ACTIVE;
        boolean willBeCounted = newModerationStatus == ModerationStatus.NONE
                && newVisibilityStatus == VisibilityStatus.ACTIVE;

        reply.setVisibilityStatus(newVisibilityStatus);
        reply.setModerationStatus(newModerationStatus);
        BigInteger delta = BigInteger.valueOf((willBeCounted ? 1 : 0) - (wasCounted ? 1 : 0));
        comment.setNumberOfReplies(comment.getNumberOfReplies().add(delta));
        commentService.saveComment(comment);
        return ReplyMapper.toDTO(reply);
    }

    @Transactional
    private ReplyDTO changeReplyStatus(Long userId,Long replyId,BigInteger postCounterChange,VisibilityStatus visibilityStatus,ModerationStatus moderationStatus,Boolean deleted){
        var reply = getReplyEntityById(replyId);
        accessibilityService.validateModeration(reply,userId);
        accessibilityService.validateVisibility(reply,userId);
        var user = userService.getUserEntityById(userId);
        var comment = commentService.getCommentEntityById(reply.getComment().getId());

        Optional.ofNullable(visibilityStatus).ifPresent(reply::setVisibilityStatus);
        Optional.ofNullable(moderationStatus).ifPresent(reply::setModerationStatus);
        Optional.ofNullable(deleted).ifPresent(reply::setDeleted);
        if(Boolean.TRUE.equals(deleted)){
            reply.setDeletedAt(LocalDateTime.now());
        }
        if(Boolean.FALSE.equals(deleted)){
            reply.setDeletedAt(null);
        }
        reply.setUpdatedAt(LocalDateTime.now());
        comment.setNumberOfReplies(comment.getNumberOfReplies().add(postCounterChange));
        commentService.saveComment(comment);
        return ReplyMapper.toDTO(replyRepository.save(reply));
    }
    @CachePut(value = "repliesByUserAndReplyIDs",key = "#userId + '-' + #replyId")
    public ReplyDTO activeReply(Long userId, Long replyId){
        return changeReplyStatus(userId,replyId,BigInteger.ONE,VisibilityStatus.ACTIVE,ModerationStatus.NONE,false);
    }
    @CachePut(value = "repliesByUserAndReplyIDs",key = "#userId + '-' + #replyId")
    public ReplyDTO hideReply(Long userId, Long replyId){
        return changeReplyStatus(userId,replyId,BigInteger.valueOf(-1),VisibilityStatus.HIDDEN,null,false);
    }
    @CacheEvict(value = "repliesByUserAndReplyIDs",key = "#userId + '-' + #replyId")
    public ReplyDTO removeByAdminReply(Long userId, Long replyId){
        return changeReplyStatus(userId,replyId,BigInteger.valueOf(-1),VisibilityStatus.REMOVED_BY_ADMIN,null,true);
    }
    @CacheEvict(value = "repliesByUserAndReplyIDs",key = "#userId + '-' + #replyId")
    public ReplyDTO deleteReply(Long userId, Long replyId){
        return changeReplyStatus(userId,replyId,BigInteger.valueOf(-1),VisibilityStatus.DELETED,ModerationStatus.NONE,true);
    }
    @CachePut(value = "repliesByUserAndReplyIDs",key = "#userId + '-' + #replyId")
    public ReplyDTO setReplyAsReported(Long userId,Long replyId){
        return changeReplyStatus(userId,replyId,BigInteger.valueOf(-1),VisibilityStatus.HIDDEN,ModerationStatus.REPORTED,false);
    }
    @CachePut(value = "repliesByUserAndReplyIDs",key = "#userId + '-' + #replyId")
    public ReplyDTO setReplyAsUnderReview(Long userId,Long replyId){
        return changeReplyStatus(userId,replyId,BigInteger.valueOf(-1),VisibilityStatus.HIDDEN,ModerationStatus.UNDER_REVIEW,false);
    }
    @CachePut(value = "repliesByUserAndReplyIDs",key = "#userId + '-' + #replyId")
    public ReplyDTO setReplyAsPendingApproval(Long userId,Long replyId){
        return changeReplyStatus(userId,replyId,BigInteger.valueOf(-1),VisibilityStatus.HIDDEN,ModerationStatus.PENDING_APPROVAL,false);
    }
    @CacheEvict(value = "repliesByUserAndReplyIDs",key = "#userId + '-' + #replyId")
    public ReplyDTO setReplyAsRejected(Long userId,Long replyId){
        return changeReplyStatus(userId,replyId,BigInteger.valueOf(-1),VisibilityStatus.REMOVED_BY_ADMIN,ModerationStatus.REJECTED,true);
    }

    public void saveReply(Reply reply){
        replyRepository.save(reply);
    }
}
