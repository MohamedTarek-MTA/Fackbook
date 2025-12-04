package com.fackbook.Post.Service;


import com.fackbook.Friend.Service.FriendService;
import com.fackbook.Group.Entity.Group;
import com.fackbook.Group.Service.GroupMemberService;
import com.fackbook.Group.Service.GroupService;
import com.fackbook.Post.DTO.PostDTO;
import com.fackbook.Post.Entity.Post;
import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Enum.Privacy;
import com.fackbook.Post.Enum.VisibilityStatus;
import com.fackbook.Post.Mapper.PostMapper;
import com.fackbook.Post.Repository.PostRepository;
import com.fackbook.Post.Util.Service.AccessibilityService;
import com.fackbook.Post.Util.Service.MediaManager;
import com.fackbook.Shared.Helper.FileHelper;
import com.fackbook.User.Enum.Role;
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
public class PostService {
    private final PostRepository postRepository;
    private final UserService userService;
    private final FileHelper fileHelper;
    private final GroupService groupService;
    private final AccessibilityService accessibilityService;
    private final FriendService friendService;
    private final GroupMemberService groupMemberService;
    private final MediaManager mediaManager;

    public Post getPostEntityById(Long id){
        return postRepository.findById(id).orElseThrow(()->
                new IllegalArgumentException("Post Not Found !"));
    }
    public Page<PostDTO> getPostsByUserId(Long userId, Pageable pageable){
        return postRepository.findByUser_Id(userId,pageable).map(
                    post -> {
                        accessibilityService.validateVisibility(post,userId);
                        accessibilityService.validateModeration(post,userId);
                        validatePrivacy(userId,post);
                       return PostMapper.toDTO(post);
                    }
        );
    }
    public Page<PostDTO> getPostsByUserName(Long userId,String userName, Pageable pageable){
        return postRepository.findByUser_NameContainingIgnoreCase(userName,pageable).map(
                post -> {
                    accessibilityService.validateVisibility(post,userId);
                    accessibilityService.validateModeration(post,userId);
                    validatePrivacy(userId,post);
                    return PostMapper.toDTO(post);
                }
        );
    }

    public Page<PostDTO> getPostsByGroupId(Long userId,Long groupId,Pageable pageable){
        return postRepository.findByGroup_Id(groupId,pageable).map(
                post -> {
                    accessibilityService.validateVisibility(post,userId);
                    accessibilityService.validateModeration(post,userId);
                    validatePrivacy(userId,post);
                    return PostMapper.toDTO(post);
                }
        );
    }
    public Page<PostDTO> getPostsByUserIdAndGroupId(Long userId,Long groupId,Pageable pageable){
        return postRepository.findByUser_IdAndGroup_Id(userId,groupId,pageable).map(
                post -> {
                    accessibilityService.validateVisibility(post,userId);
                    accessibilityService.validateModeration(post,userId);
                    validatePrivacy(userId,post);
                    return PostMapper.toDTO(post);
                }
        );
    }
    public Page<PostDTO> getPostsByGroupName(Long userId,String groupName,Pageable pageable){
        return postRepository.findByGroup_NameIgnoreCase(groupName,pageable).map(
                post -> {
                    accessibilityService.validateVisibility(post,userId);
                    accessibilityService.validateModeration(post,userId);
                    validatePrivacy(userId,post);
                    return PostMapper.toDTO(post);
                }
        );
    }
    private Page<PostDTO> getPostsByVisibilityStatus(Long userId,VisibilityStatus visibilityStatus,Pageable pageable){
        return postRepository.findByVisibilityStatus(visibilityStatus,pageable).map(
                post -> {
                    accessibilityService.validateVisibility(post,userId);
                    accessibilityService.validateModeration(post,userId);
                    validatePrivacy(userId,post);
                    return PostMapper.toDTO(post);
                }
        );
    }
    private Page<PostDTO> getPostsByModerationStatus(Long userId,ModerationStatus moderationStatus,Pageable pageable){
        return postRepository.findByModerationStatus(moderationStatus,pageable).map(
                post -> {
                    accessibilityService.validateVisibility(post,userId);
                    accessibilityService.validateModeration(post,userId);
                    validatePrivacy(userId,post);
                    return PostMapper.toDTO(post);
                }
        );
    }

    private Page<PostDTO> getPostsByPrivacy(Long userId,Privacy privacy,Pageable pageable){
        return postRepository.findByPrivacy(privacy,pageable).map(
                post -> {
                    accessibilityService.validateVisibility(post,userId);
                    accessibilityService.validateModeration(post,userId);
                    validatePrivacy(userId,post);
                    return PostMapper.toDTO(post);
                }
        );
    }

    public Page<PostDTO> getActivePosts(Long userId,Pageable pageable){
        return getPostsByVisibilityStatus(userId,VisibilityStatus.ACTIVE,pageable);
    }
    public Page<PostDTO> getHiddenPosts(Long userId,Pageable pageable){
        return getPostsByVisibilityStatus(userId,VisibilityStatus.HIDDEN,pageable);
    }
    public Page<PostDTO> getDeletedPosts(Long userId,Pageable pageable){
        return getPostsByVisibilityStatus(userId,VisibilityStatus.DELETED,pageable);
    }
    public Page<PostDTO> getRemovedByAdminPosts(Long userId,Pageable pageable){
        return getPostsByVisibilityStatus(userId,VisibilityStatus.REMOVED_BY_ADMIN,pageable);
    }
    public Page<PostDTO> getReportedPosts(Long userId,Pageable pageable){
        return getPostsByModerationStatus(userId,ModerationStatus.REPORTED,pageable);
    }
    public Page<PostDTO> getUnderReviewPosts(Long userId,Pageable pageable){
        return getPostsByModerationStatus(userId,ModerationStatus.UNDER_REVIEW,pageable);
    }
    public Page<PostDTO> getPendingApprovalPosts(Long userId,Pageable pageable){
        return getPostsByModerationStatus(userId,ModerationStatus.PENDING_APPROVAL,pageable);
    }
    public Page<PostDTO> getRejectedPosts(Long userId,Pageable pageable){
        return getPostsByModerationStatus(userId,ModerationStatus.REJECTED,pageable);
    }

    public Page<PostDTO> getPublicPosts(Long userId,Pageable pageable){
        return getPostsByPrivacy(userId,Privacy.PUBLIC,pageable);
    }
    public Page<PostDTO> getFriendsOnlyPosts(Long userId,Pageable pageable){
        return getPostsByPrivacy(userId,Privacy.FRIENDS_ONLY,pageable);
    }
    public Page<PostDTO> getPrivatePosts(Long userId,Pageable pageable){
        return getPostsByPrivacy(userId,Privacy.PRIVATE,pageable);
    }
    public Page<PostDTO> getGroupPublicPosts(Long userId,Pageable pageable){
        return getPostsByPrivacy(userId,Privacy.GROUP_PUBLIC,pageable);
    }
    public Page<PostDTO> getGroupMembersPosts(Long userId,Pageable pageable){
        return getPostsByPrivacy(userId,Privacy.GROUP_MEMBERS,pageable);
    }
    @Cacheable(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO getPostById(Long userId,Long postId){
        var post = getPostEntityById(postId);
        accessibilityService.validateModeration(post,userId);
        accessibilityService.validateVisibility(post,userId);
        validatePrivacy(userId,post);
        return PostMapper.toDTO(post);
    }
    @Transactional
    public PostDTO createNewPost(Long userId, Long groupId , PostDTO dto, MultipartFile image , MultipartFile video){
        var user = userService.getUserEntityById(userId);
        Group group = null;
        String imageUrl = fileHelper.generateImageUrl(image);
        String videoUrl = fileHelper.generateVideoUrl(video,"Uploaded By "+user.getName()+" @"+user.getId(),"Description: "+dto.getContent());
        boolean isContentNull = dto.getContent() == null || dto.getContent().isBlank();
        boolean isImageNull = imageUrl == null || imageUrl.isBlank();
        boolean isVideoNull = videoUrl == null || videoUrl.isBlank();
        if(isContentNull && isImageNull && isVideoNull){
            throw new IllegalArgumentException("Post Couldn't be empty !!");
        }
        if(groupId != null){
             group = groupService.getGroupEntityByGroupId(groupId);
        }
        Post post = Post.builder()
                .user(user)
                .group(group)
                .imageUrl(imageUrl)
                .videoUrl(videoUrl)
                .createdAt(LocalDateTime.now())
                .content(dto.getContent())
                .deleted(false)
                .deletedAt(null)
                .numberOfShares(BigInteger.ZERO)
                .numberOfComments(BigInteger.ZERO)
                .numberOfReacts(BigInteger.ZERO)
                .moderationStatus(group != null ? ModerationStatus.PENDING_APPROVAL : ModerationStatus.NONE)
                .visibilityStatus(dto.getVisibilityStatus() != null ? dto.getVisibilityStatus():VisibilityStatus.ACTIVE)
                .privacy(dto.getPrivacy())
                .build();
        return PostMapper.toDTO(postRepository.save(post));
    }

    @Transactional
    private PostDTO changePostPrivacy(Long userId,Long postId,Privacy privacy){
        var user = userService.getUserEntityById(userId);
        var post = getPostEntityById(postId);
        accessibilityService.validateModeration(post,userId);
        accessibilityService.validateVisibility(post,userId);
        validatePrivacy(userId,post);
        if(!user.getId().equals(post.getUser().getId())){
            throw new IllegalArgumentException("You can't change post privacy because it's not yours !!");
        }
       Optional.ofNullable(privacy).ifPresent(post::setPrivacy);
        post.setUpdatedAt(LocalDateTime.now());
        return PostMapper.toDTO(postRepository.save(post));
    }
    @Transactional
    private PostDTO changePostStatus(Long userId,Long postId,VisibilityStatus visibilityStatus,ModerationStatus moderationStatus,Boolean deleted){
        var user = userService.getUserEntityById(userId);
        var post = getPostEntityById(postId);
        accessibilityService.validateVisibility(post,userId);
        accessibilityService.validateModeration(post,userId);
        validatePrivacy(userId,post);
        Optional.ofNullable(visibilityStatus).ifPresent(post::setVisibilityStatus);
        Optional.ofNullable(moderationStatus).ifPresent(post::setModerationStatus);
        Optional.ofNullable(deleted).ifPresent(post::setDeleted);
        if(Boolean.TRUE.equals(deleted)){
            post.setDeletedAt(LocalDateTime.now());
        }
        post.setUpdatedAt(LocalDateTime.now());
        return PostMapper.toDTO(postRepository.save(post));
    }
    @Transactional
    public PostDTO updatePost(Long userId,Long postId,Long groupId,PostDTO dto,MultipartFile image,MultipartFile video){
        var post = getPostEntityById(postId);
        var user = userService.getUserEntityById(userId);
        Group group = null;
        accessibilityService.validateVisibility(post,userId);
        accessibilityService.validateModeration(post,userId);
        validatePrivacy(userId,post);
        mediaManager.handleMedia(post,image,video,dto.getRemoveImage(),dto.getRemoveVideo());
        var imageUrl = post.getImageUrl();
        var videoUrl = post.getVideoUrl();
        boolean isContentNull = dto.getContent() == null || dto.getContent().isBlank();
        boolean isImageNull = imageUrl == null || imageUrl.isBlank();
        boolean isVideoNull = videoUrl == null || videoUrl.isBlank();
        if(isContentNull && isImageNull && isVideoNull){
            throw new IllegalArgumentException("Post Couldn't be empty !!");
        }
        if(groupId != null){
            group = groupService.getGroupEntityByGroupId(groupId);
        }
        Optional.ofNullable(group).ifPresent(post::setGroup);
        Optional.ofNullable(dto.getContent()).ifPresent(post::setContent);
        Optional.ofNullable(dto.getPrivacy()).ifPresent(post::setPrivacy);
        post.setVisibilityStatus(dto.getVisibilityStatus() != null ? dto.getVisibilityStatus() : VisibilityStatus.ACTIVE);
        post.setModerationStatus(group != null ? ModerationStatus.PENDING_APPROVAL : ModerationStatus.NONE);
        post.setUpdatedAt(LocalDateTime.now());
        return PostMapper.toDTO(postRepository.save(post));
    }
    @CachePut(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
   public PostDTO toPublicPost(Long userId,Long postId){
        return changePostPrivacy(userId,postId,Privacy.PUBLIC);
   }
    @CachePut(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO toPrivatePost(Long userId,Long postId){
        return changePostPrivacy(userId,postId,Privacy.PRIVATE);
    }
    @CachePut(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO toOnlyFriendsPost(Long userId,Long postId){
        return changePostPrivacy(userId,postId,Privacy.FRIENDS_ONLY);
    }
    @CachePut(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO toGroupPublicPost(Long userId,Long postId){
        return changePostPrivacy(userId,postId,Privacy.GROUP_PUBLIC);
    }
    @CachePut(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO toGroupMembersPost(Long userId,Long postId){
        return changePostPrivacy(userId,postId,Privacy.GROUP_MEMBERS);
    }
    @CachePut(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO activePost(Long userId,Long postId){
        return changePostStatus(userId,postId,VisibilityStatus.ACTIVE,ModerationStatus.NONE,false);
    }
    @CachePut(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO hiddenPost(Long userId,Long postId){
        return changePostStatus(userId,postId,VisibilityStatus.HIDDEN,null,false);
    }
    @CacheEvict(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO removeByAdminPost(Long userId,Long postId){
        return changePostStatus(userId,postId,VisibilityStatus.REMOVED_BY_ADMIN,null,true);
    }
    @CacheEvict(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO deletedPost(Long userId,Long postId){
        return changePostStatus(userId,postId,VisibilityStatus.DELETED,ModerationStatus.NONE,true);
    }
    @CachePut(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO reportedPost(Long userId,Long postId){
        return changePostStatus(userId,postId,VisibilityStatus.HIDDEN,ModerationStatus.REPORTED,false);
    }
    @CachePut(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO underReviewPost(Long userId,Long postId){
        return changePostStatus(userId,postId,VisibilityStatus.HIDDEN,ModerationStatus.UNDER_REVIEW,false);
    }
    @CachePut(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO pendingApprovalPost(Long userId,Long postId){
        return changePostStatus(userId,postId,VisibilityStatus.HIDDEN,ModerationStatus.PENDING_APPROVAL,false);
    }
    @CacheEvict(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO rejectedPost(Long userId,Long postId){
        return changePostStatus(userId,postId,VisibilityStatus.REMOVED_BY_ADMIN,ModerationStatus.REJECTED,true);
    }

    private void validatePrivacy(Long userId, Post post) {

        var user = userService.getUserEntityById(userId);


        Long authorId = post.getAuthorId();
        Long groupOwnerId = post.getGroupOwnerId();

        var friendship = friendService.getFriendshipEntityByUserIdAndFriendId(authorId, userId);
        var groupMember = groupMemberService.getGroupMemberEntityByUserIdAndGroupId(userId, groupOwnerId);

        boolean isAuthor = userId.equals(authorId);
        boolean isSystemAdmin = user.getRole() == Role.SYSTEM_ADMIN;
        boolean isGroupAdmin = groupOwnerId != null
                && groupOwnerId.equals(userId)
                && user.getRole() == Role.GROUP_ADMIN;


        if (isAuthor || isSystemAdmin || isGroupAdmin) return;


        switch (post.getPrivacy()) {

            case PUBLIC, GROUP_PUBLIC -> {
                return;
            }

            case FRIENDS_ONLY -> {
                if (friendship.isPresent()) return;
                break;
            }

            case PRIVATE -> {

                break;
            }

            case GROUP_MEMBERS -> {
                if (groupMember.isPresent()) return;
                break;
            }

            default -> throw new IllegalArgumentException(
                    "Unknown privacy type: " + post.getPrivacy()
            );
        }

        throw new IllegalArgumentException("You are not allowed to view this post.");
    }

    public void savePost(Post post){
        postRepository.save(post);
    }
}
