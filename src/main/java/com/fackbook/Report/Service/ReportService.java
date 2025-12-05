package com.fackbook.Report.Service;

import com.fackbook.Comment.Repository.CommentRepository;
import com.fackbook.Friend.Repository.FriendshipRepository;
import com.fackbook.Group.Repository.GroupMemberRepository;
import com.fackbook.Group.Repository.GroupRepository;
import com.fackbook.Post.Enum.VisibilityStatus;
import com.fackbook.Post.Repository.PostRepository;
import com.fackbook.Reply.Entity.Reply;
import com.fackbook.Reply.Repository.ReplyRepository;
import com.fackbook.Report.DTO.ReportDTO;
import com.fackbook.Report.Entity.Report;
import com.fackbook.Report.Mapper.ReportMapper;
import com.fackbook.Report.Repository.ReportRepository;
import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.Shared.Helper.FileHelper;
import com.fackbook.User.Enum.Status;
import com.fackbook.User.Repository.UserRepository;
import com.fackbook.User.Service.UserService;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springdoc.core.converters.PageableOpenAPIConverter;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.time.LocalDateTime;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class ReportService {
    private final ReportRepository reportRepository;
    private final UserRepository userRepository;
    private final GroupRepository groupRepository;
    private final GroupMemberRepository groupMemberRepository;
    private final PostRepository postRepository;
    private final CommentRepository commentRepository;
    private final ReplyRepository replyRepository;
    private final FileHelper fileHelper;

    private Long resolveTargetId(RequestTargetType targetType,Long targetId,Long groupId){
       return switch (targetType){
           case USER -> {
                var user = userRepository.findById(targetId).orElseThrow(()->new IllegalArgumentException("User Not Found !"));
                if(user.getStatus().equals(Status.BANNED) || user.getStatus().equals(Status.DELETED))
                    throw new IllegalArgumentException("User is already "+user.getStatus().name());
                yield user.getId();
            }
            case GROUP -> {
                var group = groupRepository.findById(targetId).orElseThrow(()->new IllegalArgumentException("Group Not Found !"));
                if(group.getStatus().equals(Status.BANNED) || group.getStatus().equals(Status.DELETED))
                    throw new IllegalArgumentException("Group is already "+group.getStatus().name());
                yield group.getId();
            }
           case GROUP_MEMBER -> {
               if(groupId == null)
                   throw new IllegalArgumentException("Group Not Found !");
               var group = groupRepository.findById(groupId);
               if(group.isPresent()){
                   var groupMember  = groupMemberRepository.findByUser_IdAndGroup_Id(targetId,groupId);
                   if(groupMember.isPresent()){
                       var existGroupMember = groupMember.get();
                       if(existGroupMember.getStatus().equals(Status.DELETED) || existGroupMember.getStatus().equals(Status.BANNED))
                           throw new IllegalArgumentException("Group Member is already "+existGroupMember.getStatus().name());
                       yield existGroupMember.getId();
                   }
               }
               throw new IllegalArgumentException("Group Not Found !");
           }
           case POST -> {
               var post = postRepository.findById(targetId).orElseThrow(()->new IllegalArgumentException("Post Not Found !"));
               if(post.getVisibilityStatus().equals(VisibilityStatus.DELETED)||post.getVisibilityStatus().equals(VisibilityStatus.REMOVED_BY_ADMIN))
                   throw new IllegalArgumentException("Post is already "+post.getVisibilityStatus().name());
               yield post.getId();
           }
           case COMMENT -> {
               var comment = commentRepository.findById(targetId).orElseThrow(()->new IllegalArgumentException("Comment Not Found !"));
               if(comment.getVisibilityStatus().equals(VisibilityStatus.DELETED)||comment.getVisibilityStatus().equals(VisibilityStatus.REMOVED_BY_ADMIN))
                   throw new IllegalArgumentException("Comment is already "+comment.getVisibilityStatus().name());
               yield comment.getId();
           }
           case REPLY -> {
               var reply = replyRepository.findById(targetId).orElseThrow(()->new IllegalArgumentException("Reply Not Found !"));
               if(reply.getVisibilityStatus().equals(VisibilityStatus.DELETED)||reply.getVisibilityStatus().equals(VisibilityStatus.REMOVED_BY_ADMIN))
                   throw new IllegalArgumentException("Reply is already "+reply.getVisibilityStatus().name());
               yield reply.getId();
           }
           default -> throw new IllegalArgumentException("Unknown target type "+targetType);
        };
    }

    public Report getReportEntityById(Long reportId){
        return reportRepository.findById(reportId).orElseThrow(()->new IllegalArgumentException("Report Not Found !"));
    }

    public ReportDTO getReport(Long reportId){
        return ReportMapper.toDTO(getReportEntityById(reportId));
    }
    public Page<ReportDTO> getReportsByUserId(Long userId, Pageable pageable){
        return reportRepository.findByUser_Id(userId,pageable).map(ReportMapper::toDTO);
    }
    private Page<ReportDTO> getReportsByTargetIdAndTargetType(Long targetId, RequestTargetType targetType, Pageable pageable){
        return reportRepository.findByTargetIdAndTargetType(targetId,targetType,pageable).map(ReportMapper::toDTO);
    }
    public Page<ReportDTO> getReportsOnUsers(Long targetId,Pageable pageable){
        return getReportsByTargetIdAndTargetType(targetId,RequestTargetType.USER,pageable);
    }
    public Page<ReportDTO> getReportsOnGroupMembers(Long targetId,Long groupId,Pageable pageable){
        var finalTargetId = resolveTargetId(RequestTargetType.GROUP_MEMBER,targetId,groupId);
        return getReportsByTargetIdAndTargetType(finalTargetId,RequestTargetType.GROUP_MEMBER,pageable);
    }
    public Page<ReportDTO> getReportsOnGroups(Long targetId,Pageable pageable){
        return getReportsByTargetIdAndTargetType(targetId,RequestTargetType.GROUP,pageable);
    }
    public Page<ReportDTO> getReportsOnPosts(Long targetId,Pageable pageable){
        return getReportsByTargetIdAndTargetType(targetId,RequestTargetType.POST,pageable);
    }
    public Page<ReportDTO> getReportsOnComments(Long targetId,Pageable pageable){
        return getReportsByTargetIdAndTargetType(targetId,RequestTargetType.COMMENT,pageable);
    }
    public Page<ReportDTO> getReportsOReplies(Long targetId,Pageable pageable){
        return getReportsByTargetIdAndTargetType(targetId,RequestTargetType.REPLY,pageable);
    }
    public Page<ReportDTO> getReportByUserIdAndTargetIdAndTargetType(Long userId,Long targetId,Long groupId,RequestTargetType targetType,Pageable pageable){
        var user = userRepository.findById(userId).orElseThrow(()->new IllegalArgumentException("User Not Found !"));
        var actualTargetId = resolveTargetId(targetType,targetId,groupId);
        return reportRepository.findByUser_IdAndTargetIdAndTargetType(userId,actualTargetId,targetType,pageable).map(ReportMapper::toDTO);
    }
    public Page<ReportDTO> findByReviewerId(Long reviewerId,Pageable pageable){
        var reviewer = userRepository.findById(reviewerId).orElseThrow(()->new IllegalArgumentException("Reviewer Not Found !"));
        return reportRepository.findByReviewerId(reviewerId,pageable).map(ReportMapper::toDTO);
    }
    @Transactional
    public ReportDTO createNewReport(Long userId, Long targetId,Long groupId, ReportDTO dto, MultipartFile image,MultipartFile video){
        var user = userRepository.findById(userId).orElseThrow(()->new IllegalArgumentException("User Not Found !"));
        var actualTargetId = resolveTargetId(dto.getTargetType(),targetId,groupId);
        var imageUrl = fileHelper.generateImageUrl(image);
        var videoUrl = fileHelper.generateVideoUrl(video,"Uploaded By "+user.getName()+" @"+user.getId(),"Description: "+dto.getContent());
        boolean isContentNull = dto.getContent() == null || dto.getContent().isBlank();
        boolean isImageNull = imageUrl == null || imageUrl.isBlank();
        boolean isVideoNull = videoUrl == null || videoUrl.isBlank();
        if(isContentNull && isImageNull && isVideoNull){
            throw new IllegalArgumentException("Report Couldn't be empty !!");
        }
        Report report = Report.builder()
                .user(user)
                .content(dto.getContent())
                .imageUrl(imageUrl)
                .videoUrl(videoUrl)
                .reviewer(null)
                .status(com.fackbook.Request.Enum.Status.PENDING)
                .createdAt(LocalDateTime.now())
                .deleted(false)
                .deletedAt(null)
                .targetId(actualTargetId)
                .targetType(dto.getTargetType())
                .build();
        return ReportMapper.toDTO(reportRepository.save(report));
    }
    @Transactional
    private ReportDTO changeReportStatus(Long reviewerId,Long reportId,com.fackbook.Request.Enum.Status status,Boolean deleted){
        var report = getReportEntityById(reportId);
        var reviewer = userRepository.findById(reviewerId).orElseThrow(()->new IllegalArgumentException("Reviewer Not Found !"));
        Optional.ofNullable(status).ifPresent(report::setStatus);
        Optional.ofNullable(deleted).ifPresent(report::setDeleted);
        if(Boolean.TRUE.equals(deleted)){
            report.setDeletedAt(LocalDateTime.now());
        }
        if(Boolean.FALSE.equals(deleted)){
            report.setDeletedAt(null);
        }
        report.setUpdatedAt(LocalDateTime.now());
        report.setReviewer(reviewer);
        return ReportMapper.toDTO(reportRepository.save(report));
    }

    public ReportDTO setReportAsAccepted(Long reviewerId,Long reportId){
        return changeReportStatus(reviewerId,reportId, com.fackbook.Request.Enum.Status.ACCEPTED,false);
    }
    public ReportDTO setReportAsPending(Long reviewerId,Long reportId){
        return changeReportStatus(reviewerId,reportId, com.fackbook.Request.Enum.Status.PENDING,false);
    }
    public ReportDTO setReportAsRejected(Long reviewerId,Long reportId){
        return changeReportStatus(reviewerId,reportId, com.fackbook.Request.Enum.Status.REJECTED,true);
    }
    public ReportDTO setReportAsResolved(Long reviewerId,Long reportId){
        return changeReportStatus(reviewerId,reportId, com.fackbook.Request.Enum.Status.RESOLVED,false);
    }
    public ReportDTO setReportAsCanceled(Long reviewerId,Long reportId){
        var report = getReportEntityById(reportId);
        if(!report.getStatus().equals(com.fackbook.Request.Enum.Status.PENDING))
            throw new IllegalArgumentException("Report Already "+report.getStatus().name());
        return changeReportStatus(reviewerId,reportId, com.fackbook.Request.Enum.Status.CANCELED,true);
    }
}
