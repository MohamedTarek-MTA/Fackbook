package com.fackbook.Request.Service;

import com.fackbook.Comment.Repository.CommentRepository;
import com.fackbook.Comment.Service.CommentService;
import com.fackbook.Friend.Repository.FriendshipRepository;
import com.fackbook.Group.Entity.GroupMember;
import com.fackbook.Group.Enum.JoinPolicy;
import com.fackbook.Group.Repository.GroupMemberRepository;
import com.fackbook.Group.Repository.GroupRepository;
import com.fackbook.Group.Service.GroupMemberService;
import com.fackbook.Notification.NotificationService;
import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Repository.PostRepository;
import com.fackbook.Post.Service.PostService;
import com.fackbook.Request.DTO.RequestDTO;
import com.fackbook.Request.Entity.Request;
import com.fackbook.Request.Enum.RequestActionType;
import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.Request.Enum.Status;
import com.fackbook.Request.Mapper.RequestMapper;
import com.fackbook.Request.Repository.RequestRepository;
import com.fackbook.User.Entity.User;
import com.fackbook.User.Enum.Role;
import com.fackbook.User.Service.UserService;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

@Service
@RequiredArgsConstructor
public class RequestService {
    private final UserService userService;
    private final GroupRepository groupRepository;
    private final GroupMemberRepository groupMemberRepository;
    private final FriendshipRepository friendshipRepository;
    private final PostRepository postRepository;
    private final RequestRepository requestRepository;
    private final CommentRepository commentRepository;
    private final GroupMemberService groupMemberService;
    private final NotificationService notificationService;
    @Transactional
    public Request createNewRequest(Long userId, Long targetId, RequestDTO dto){
        var action = dto.getActionType();
        if(userId.equals(targetId) && dto.getActionType().equals(RequestActionType.FRIENDSHIP_REQUEST)){
            throw new IllegalArgumentException("You can't send a friend request to yourself.");
        }
        var user = userService.getUserEntityById(userId);
        var resolvedTargetId = resolveTargetId(action,targetId,userId);

        Request request = Request.builder()
                .user(user)
                .targetId(resolvedTargetId)
                .targetType(dto.getTargetType())
                .actionType(action)
                .status(Status.PENDING)
                .createdAt(LocalDateTime.now())
                .deleted(false)
                .deletedAt(null)
                .build();
        requestRepository.save(request);
        notificationService.sendNotificationViaRequest(request);
        return request;
    }
    private Long resolveTargetId(RequestActionType action, Long targetId, Long userId) {

        return switch(action) {
            case GROUP_JOIN_REQUEST, GROUP_INVITE -> {
                var group = groupRepository.findById(targetId).orElseThrow(()->new IllegalArgumentException("Group Not Found !"));
                if(group.getJoinPolicy() == JoinPolicy.PUBLIC)
                    throw new IllegalArgumentException("This group does not require join request.");
                yield group.getId();
            }

            case FRIENDSHIP_REQUEST -> {
                var friend = userService.getUserEntityById(targetId);
                var friendship = friendshipRepository.findByUser_IdAndFriend_Id(userId, targetId);

                if(friendship.isPresent() && friendship.get().getStatus().equals(com.fackbook.Friend.Enum.Status.FRIENDS))
                    throw new IllegalArgumentException("Friendship already exists!");

                yield friend.getId();
            }

            case CONTENT_APPROVAL -> {
                var post = postRepository.findById(targetId).orElseThrow(()->new IllegalArgumentException("Post Not Found !"));
                yield post.getId();
            }

            default -> throw new IllegalArgumentException("Unsupported request type: " + action);
        };
    }
    public Request getRequestEntityById(Long requestId){
        return requestRepository.findById(requestId).orElseThrow(()->new IllegalArgumentException("Request Not Found !"));
    }
    public RequestDTO getRequestById(Long requestId){
        return RequestMapper.toDTO(getRequestEntityById(requestId));
    }

    /**
     * Returns the request only if the current user is allowed to access it.
     * Allowed: system admin, sender, receiver (friend / group owner or admin / invited user).
     * @throws IllegalArgumentException if request not found or access denied
     */
    public RequestDTO getRequestByIdIfCanAccess(Long requestId, Long currentUserId){
        Request request = getRequestEntityById(requestId);
        if (!canAccessRequest(request, currentUserId)) {
            throw new IllegalArgumentException("You do not have permission to access this request.");
        }
        return RequestMapper.toDTO(request);
    }

    /**
     * Returns a page of requests visible to the current user only:
     * - System admin: all requests
     * - Otherwise: requests sent by user, or where user is receiver (friend request target, group join/invite for their groups or invites to them)
     */
    public Page<RequestDTO> getRequestsVisibleToUser(Long currentUserId, Pageable pageable){
        User currentUser = userService.getUserEntityById(currentUserId);
        if (currentUser.getRole() == Role.SYSTEM_ADMIN) {
            return requestRepository.findAll(pageable).map(RequestMapper::toDTO);
        }
        List<Long> groupIds = getGroupIdsWhereUserIsOwnerOrAdmin(currentUserId);
        if (groupIds.isEmpty()) {
            groupIds = List.of(-1L); // so IN :groupIds does not match any real group
        }
        return requestRepository.findVisibleToUser(currentUserId, groupIds, pageable).map(RequestMapper::toDTO);
    }

    /**
     * Checks whether the current user can view/act on this request.
     * Allowed: system admin, sender, receiver (friend / group owner or admin / invited user).
     */
    public boolean canAccessRequest(Request request, Long currentUserId){
        User currentUser = userService.getUserEntityById(currentUserId);
        if (currentUser.getRole() == Role.SYSTEM_ADMIN) {
            return true;
        }
        if (request.getUser().getId().equals(currentUserId)) {
            return true; // sender
        }
        switch (request.getActionType()) {
            case FRIENDSHIP_REQUEST:
                return request.getTargetType() == RequestTargetType.USER && request.getTargetId().equals(currentUserId);
            case GROUP_JOIN_REQUEST:
            case GROUP_INVITE:
                if (request.getTargetType() != RequestTargetType.GROUP) return false;
                List<Long> groupIds = getGroupIdsWhereUserIsOwnerOrAdmin(currentUserId);
                return groupIds.contains(request.getTargetId());
            case CONTENT_APPROVAL:
                return false;
            default:
                return false;
        }
    }

    private List<Long> getGroupIdsWhereUserIsOwnerOrAdmin(Long userId){
        List<Long> ownerIds = groupRepository.findByUser_Id(userId, Pageable.unpaged()).getContent().stream()
                .map(g -> g.getId())
                .toList();
        List<Long> adminGroupIds = groupMemberRepository.findByUser_IdAndRole(userId, Role.GROUP_ADMIN, Pageable.unpaged())
                .getContent().stream()
                .map(GroupMember::getGroup)
                .map(g -> g.getId())
                .distinct()
                .toList();
        return Stream.concat(ownerIds.stream(), adminGroupIds.stream()).distinct().toList();
    }

    /** True if current user is allowed to accept or reject this request (receiver or system admin). */
    public boolean canAcceptOrRejectRequest(Request request, Long currentUserId){
        User currentUser = userService.getUserEntityById(currentUserId);
        if (currentUser.getRole() == Role.SYSTEM_ADMIN) return true;
        if (request.getUser().getId().equals(currentUserId)) return false; // sender cannot accept/reject
        return canAccessRequest(request, currentUserId); // receiver can
    }

    /** True if current user is allowed to cancel this request (sender or system admin). */
    public boolean canCancelRequest(Request request, Long currentUserId){
        User currentUser = userService.getUserEntityById(currentUserId);
        if (currentUser.getRole() == Role.SYSTEM_ADMIN) return true;
        return request.getUser().getId().equals(currentUserId);
    }

    /** Accept request; throws if current user is not allowed to accept (receiver or system admin). */
    public RequestDTO setRequestAsAcceptedIfCanAccess(Long requestId, Long currentUserId){
        Request request = getRequestEntityById(requestId);
        if (!canAcceptOrRejectRequest(request, currentUserId)) {
            throw new IllegalArgumentException("You do not have permission to accept this request.");
        }
        return setRequestAsAccepted(requestId);
    }

    /** Reject request; throws if current user is not allowed to reject (receiver or system admin). */
    public RequestDTO setRequestAsRejectedIfCanAccess(Long requestId, Long currentUserId){
        Request request = getRequestEntityById(requestId);
        if (!canAcceptOrRejectRequest(request, currentUserId)) {
            throw new IllegalArgumentException("You do not have permission to reject this request.");
        }
        return setRequestAsRejected(requestId);
    }

    /** Cancel request; throws if current user is not allowed to cancel (sender or system admin). */
    public RequestDTO setRequestAsCanceledIfCanAccess(Long requestId, Long currentUserId){
        Request request = getRequestEntityById(requestId);
        if (!canCancelRequest(request, currentUserId)) {
            throw new IllegalArgumentException("You do not have permission to cancel this request.");
        }
        return setRequestAsCanceled(requestId);
    }

    public Page<RequestDTO> getRequestsByUserId(Long userId, Pageable pageable){
        return requestRepository.findByUser_Id(userId,pageable).map(RequestMapper::toDTO);
    }
    public RequestDTO getRequestByUserIdAndTargetIdAndActionType(Long userId,Long targetId,RequestActionType actionType){
        return RequestMapper.toDTO(
                requestRepository.findByUser_IdAndTargetIdAndActionType(userId,targetId,actionType).orElseThrow(
                        ()->new IllegalArgumentException("Request Not Found !"))
        );
    }
    public Page<RequestDTO> getRequestByTargetIdAndActionType(Long targetId,RequestActionType actionType,Pageable pageable){
        return requestRepository.findByTargetIdAndActionType(targetId,actionType,pageable).map(RequestMapper::toDTO);
    }
    private Page<RequestDTO> getRequestsByActionType(RequestActionType actionType,Pageable pageable){
        return requestRepository.findByActionType(actionType,pageable).map(RequestMapper::toDTO);
    }
    private Page<RequestDTO> getRequestsByTargetType(RequestTargetType targetType, Pageable pageable){
        return requestRepository.findByTargetType(targetType,pageable).map(RequestMapper::toDTO);
    }
    private Page<RequestDTO> getRequestsByStatus(Status status,Pageable pageable){
        return requestRepository.findByStatus(status,pageable).map(RequestMapper::toDTO);
    }

    public Page<RequestDTO> getFriendShipRequests(Pageable pageable){
        return getRequestsByActionType(RequestActionType.FRIENDSHIP_REQUEST,pageable);
    }
    public Page<RequestDTO> getGroupJoinRequests(Pageable pageable){
        return getRequestsByActionType(RequestActionType.GROUP_JOIN_REQUEST,pageable);
    }
    public Page<RequestDTO> getGroupInviteRequests(Pageable pageable){
        return getRequestsByActionType(RequestActionType.GROUP_INVITE,pageable);
    }
    public Page<RequestDTO> getContentApprovalRequests(Pageable pageable){
        return getRequestsByActionType(RequestActionType.CONTENT_APPROVAL,pageable);
    }
    public Page<RequestDTO> getUserRequests(Pageable pageable){
        return getRequestsByTargetType(RequestTargetType.USER,pageable);
    }
    public Page<RequestDTO> getGroupRequests(Pageable pageable){
        return getRequestsByTargetType(RequestTargetType.GROUP,pageable);
    }
    public Page<RequestDTO> getGroupMemberRequests(Pageable pageable){
        return getRequestsByTargetType(RequestTargetType.GROUP_MEMBER,pageable);
    }
    public Page<RequestDTO> getPostRequests(Pageable pageable){
        return getRequestsByTargetType(RequestTargetType.POST,pageable);
    }
    public Page<RequestDTO> getCommentRequests(Pageable pageable){
        return getRequestsByTargetType(RequestTargetType.COMMENT,pageable);
    }
    public Page<RequestDTO> getReplyRequests(Pageable pageable){
        return getRequestsByTargetType(RequestTargetType.REPLY,pageable);
    }
    public Page<RequestDTO> getPendingRequests(Pageable pageable){
        return getRequestsByStatus(Status.PENDING,pageable);
    }
    public Page<RequestDTO> getAcceptedRequests(Pageable pageable){
        return getRequestsByStatus(Status.ACCEPTED,pageable);
    }
    public Page<RequestDTO> getRejectedRequests(Pageable pageable){
        return getRequestsByStatus(Status.REJECTED,pageable);
    }
    public Page<RequestDTO> getCanceledRequests(Pageable pageable){
        return getRequestsByStatus(Status.CANCELED,pageable);
    }

    @Transactional
    private RequestDTO changeRequestStatus(Long requestId,Status status,Boolean deleted){
        var request = getRequestEntityById(requestId);
        Optional.ofNullable(status).ifPresent(request::setStatus);
        Optional.ofNullable(deleted).ifPresent(request::setDeleted);
        if(Boolean.TRUE.equals(deleted)){
            request.setDeletedAt(LocalDateTime.now());
        }
        if(Boolean.FALSE.equals(deleted)){
            request.setDeletedAt(null);
        }
        request.setUpdatedAt(LocalDateTime.now());
        return RequestMapper.toDTO(requestRepository.save(request));
    }

    public RequestDTO setRequestAsAccepted(Long requestId){
        var request = getRequestEntityById(requestId);
        if(!request.getStatus().equals(Status.PENDING)){
            throw new IllegalArgumentException("Request Already "+request.getStatus().name());
        }
        var acceptedRequest = changeRequestStatus(requestId,Status.ACCEPTED,false);
        if(request.getActionType().equals(RequestActionType.GROUP_JOIN_REQUEST)){
            groupMemberService.handleGroupMembership(request.getUser().getId(),request.getTargetId(),request);
        }
        if(request.getActionType().equals(RequestActionType.CONTENT_APPROVAL)){
            if(request.getTargetType().equals(RequestTargetType.POST)){
                changeModerationStatusByPostId(request.getTargetId(), ModerationStatus.NONE);
            }
            if(request.getTargetType().equals(RequestTargetType.COMMENT)){
                changeModerationStatusByCommentId(request.getTargetId(),ModerationStatus.NONE);
            }
        }
        return acceptedRequest;
    }
    public RequestDTO setRequestAsRejected(Long requestId){
        var request = getRequestEntityById(requestId);
        if(!request.getStatus().equals(Status.PENDING)){
            throw new IllegalArgumentException("Request Already "+request.getStatus().name());
        }
        var rejectedRequest = changeRequestStatus(requestId,Status.REJECTED,true);
        // Do NOT add user to group on reject; handleGroupMembership is only for accept.
        if(request.getActionType().equals(RequestActionType.CONTENT_APPROVAL)){
            if(request.getTargetType().equals(RequestTargetType.POST)){
                changeModerationStatusByPostId(request.getTargetId(), ModerationStatus.REJECTED);
            }
            if(request.getTargetType().equals(RequestTargetType.COMMENT)){
                changeModerationStatusByCommentId(request.getTargetId(),ModerationStatus.REJECTED);
            }
        }
        return rejectedRequest;
    }
    public RequestDTO setRequestAsCanceled(Long requestId){
        var request = getRequestEntityById(requestId);
        if(!request.getStatus().equals(Status.PENDING)){
            throw new IllegalArgumentException("Request Already "+request.getStatus().name());
        }
        return changeRequestStatus(requestId,Status.CANCELED,true);
    }
    public RequestDTO setRequestAsPending(Long requestId){
        var request = getRequestEntityById(requestId);
        if(!request.getStatus().equals(Status.PENDING)){
            throw new IllegalArgumentException("Request Already "+request.getStatus().name());
        }
        return changeRequestStatus(requestId,Status.PENDING,false);
    }
    public void saveRequest(Request request){
        requestRepository.save(request);
    }

    @Transactional
    private void changeModerationStatusByCommentId(Long commentId,ModerationStatus moderationStatus){
        var comment = commentRepository.findById(commentId).orElseThrow(()->new IllegalArgumentException("Comment Not Found!"));
        comment.setModerationStatus(moderationStatus);
        comment.setUpdatedAt(LocalDateTime.now());
        commentRepository.save(comment);
    }
    @Transactional
    private void changeModerationStatusByPostId(Long postId,ModerationStatus moderationStatus){
        var post = postRepository.findById(postId).orElseThrow(()->new IllegalArgumentException("Post Not Found!"));
        post.setModerationStatus(moderationStatus);
        post.setUpdatedAt(LocalDateTime.now());
        postRepository.save(post);
    }
}
