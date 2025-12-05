package com.fackbook.Request.Service;

import com.fackbook.Friend.Repository.FriendshipRepository;
import com.fackbook.Group.Enum.JoinPolicy;
import com.fackbook.Group.Repository.GroupRepository;
import com.fackbook.Post.Repository.PostRepository;
import com.fackbook.Post.Service.PostService;
import com.fackbook.Request.DTO.RequestDTO;
import com.fackbook.Request.Entity.Request;
import com.fackbook.Request.Enum.RequestActionType;
import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.Request.Enum.Status;
import com.fackbook.Request.Mapper.RequestMapper;
import com.fackbook.Request.Repository.RequestRepository;
import com.fackbook.User.Service.UserService;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class RequestService {
    private final UserService userService;
    private final GroupRepository groupRepository;
    private final FriendshipRepository friendshipRepository;
    private final PostRepository postRepository;
    private final RequestRepository requestRepository;

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
        return requestRepository.save(request);
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
        return changeRequestStatus(requestId,Status.ACCEPTED,false);
    }
    public RequestDTO setRequestAsRejected(Long requestId){
        var request = getRequestEntityById(requestId);
        if(!request.getStatus().equals(Status.PENDING)){
            throw new IllegalArgumentException("Request Already "+request.getStatus().name());
        }
        return changeRequestStatus(requestId,Status.REJECTED,true);
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
}
