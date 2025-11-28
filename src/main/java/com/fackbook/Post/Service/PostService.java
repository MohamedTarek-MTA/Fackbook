package com.fackbook.Post.Service;

import com.fackbook.Exception.ImageUploadException;
import com.fackbook.Group.Entity.Group;
import com.fackbook.Group.Entity.GroupMember;
import com.fackbook.Group.Service.GroupMemberService;
import com.fackbook.Group.Service.GroupService;
import com.fackbook.Post.DTO.PostDTO;
import com.fackbook.Post.Entity.Post;
import com.fackbook.Post.Enum.Privacy;
import com.fackbook.Post.Enum.Status;
import com.fackbook.Post.Mapper.PostMapper;
import com.fackbook.Post.Repository.PostRepository;
import com.fackbook.Shared.Helper.FileHelper;
import com.fackbook.Shared.Image.UploadImageService;
import com.fackbook.Shared.Video.YouTubeUploadService;
import com.fackbook.User.Enum.Role;
import com.fackbook.User.Service.UserService;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
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
    private final GroupMemberService groupMemberService;

    public Post getPostEntityById(Long id){
        return postRepository.findById(id).orElseThrow(()->
                new IllegalArgumentException("Post Not Found !"));
    }
    public Page<PostDTO> getPostsByUserId(Long userId, Pageable pageable){
        return postRepository.findByUser_Id(userId,pageable).map(PostMapper::toDTO);
    }

    public Page<PostDTO> getPostsByGroupId(Long groupId,Pageable pageable){
        return postRepository.findByGroup_Id(groupId,pageable).map(PostMapper::toDTO);
    }
    public Page<PostDTO> getPostsByUserIdAndGroupId(Long userId,Long groupId,Pageable pageable){
        return postRepository.findByUser_IdAndGroup_Id(userId,groupId,pageable).map(PostMapper::toDTO);
    }
    public Page<PostDTO> getPostsByGroupName(String groupName,Pageable pageable){
        return postRepository.findByGroup_NameIgnoreCase(groupName,pageable).map(PostMapper::toDTO);
    }
    private Page<PostDTO> getPostsByStatus(Status status,Pageable pageable){
        return postRepository.findByStatus(status,pageable).map(PostMapper::toDTO);
    }
    private Page<PostDTO> getPostsByPrivacy(Privacy privacy,Pageable pageable){
        return postRepository.findByPrivacy(privacy,pageable).map(PostMapper::toDTO);
    }

    public Page<PostDTO> getActivePosts(Pageable pageable){
        return getPostsByStatus(Status.ACTIVE,pageable);
    }
    public Page<PostDTO> getHiddenPosts(Pageable pageable){
        return getPostsByStatus(Status.HIDDEN,pageable);
    }
    public Page<PostDTO> getDeletedPosts(Pageable pageable){
        return getPostsByStatus(Status.DELETED,pageable);
    }
    public Page<PostDTO> getReportedPosts(Pageable pageable){
        return getPostsByStatus(Status.REPORTED,pageable);
    }
    public Page<PostDTO> getUnderReviewPosts(Pageable pageable){
        return getPostsByStatus(Status.UNDER_REVIEW,pageable);
    }
    public Page<PostDTO> getRemovedByAdminPosts(Pageable pageable){
        return getPostsByStatus(Status.REMOVED_BY_ADMIN,pageable);
    }
    public Page<PostDTO> getPendingApprovalPosts(Pageable pageable){
        return getPostsByStatus(Status.PENDING_APPROVAL,pageable);
    }
    public Page<PostDTO> getRejectedPosts(Pageable pageable){
        return getPostsByStatus(Status.REJECTED,pageable);
    }

    public Page<PostDTO> getPublicPosts(Pageable pageable){
        return getPostsByPrivacy(Privacy.PUBLIC,pageable);
    }
    public Page<PostDTO> getFriendsOnlyPosts(Pageable pageable){
        return getPostsByPrivacy(Privacy.FRIENDS_ONLY,pageable);
    }
    public Page<PostDTO> getPrivatePosts(Pageable pageable){
        return getPostsByPrivacy(Privacy.PRIVATE,pageable);
    }
    public Page<PostDTO> getGroupPublicPosts(Pageable pageable){
        return getPostsByPrivacy(Privacy.GROUP_PUBLIC,pageable);
    }
    public Page<PostDTO> getGroupMembersPosts(Pageable pageable){
        return getPostsByPrivacy(Privacy.GROUP_MEMBERS,pageable);
    }
    @Transactional
    public PostDTO createNewPost(Long userId, Long groupId , PostDTO dto, MultipartFile image , MultipartFile video){
        var user = userService.getUserEntityById(userId);
        Group group = null;
        String imageUrl = fileHelper.generateImageUrl(image);
        String videoUrl = fileHelper.generateVideoUrl(video,"Uploaded By "+user.getName()+" @"+user.getId(),dto.getContent());
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
                .status(group == null ? dto.getStatus() :Status.UNDER_REVIEW)
                .privacy(dto.getPrivacy())
                .build();
        return PostMapper.toDTO(postRepository.save(post));
    }

    @Transactional
    private PostDTO changePostPrivacy(Long userId,Long postId,Privacy privacy){
        var user = userService.getUserEntityById(userId);
        var post = getPostEntityById(postId);
        if(!user.getId().equals(post.getUser().getId())){
            throw new IllegalArgumentException("You can't change post privacy because it's not yours !!");
        }
       Optional.ofNullable(privacy).ifPresent(post::setPrivacy);
        post.setUpdatedAt(LocalDateTime.now());
        return PostMapper.toDTO(postRepository.save(post));
    }
    @Transactional
    private PostDTO changePostStatus(Long userId,Long postId,Status status,Boolean deleted){
        var user = userService.getUserEntityById(userId);
        var post = getPostEntityById(postId);
        Group group = new Group();
        Optional<GroupMember> groupMember = Optional.empty();
        if(post.getGroup() != null){
            group = groupService.getGroupEntityByGroupId(post.getGroup().getId());
            groupMember = groupMemberService.getGroupMemberEntityByUserIdAndGroupId(user.getId(),group.getId());
        }
        if(!user.getId().equals(post.getUser().getId())){
            if(groupMember.isPresent()){
                var exitsGroupMember = groupMember.get();
                if (!exitsGroupMember.getRole().equals(Role.GROUP_ADMIN) && !user.getRole().equals(Role.SYSTEM_ADMIN)){
                    throw new IllegalArgumentException("You Don't hava the permission to perform this action !!");
                }else{
                    Optional.ofNullable(status).ifPresent(post::setStatus);
                    Optional.ofNullable(deleted).ifPresent(post::setDeleted);
                    if(Boolean.TRUE.equals(deleted)){
                        post.setDeletedAt(LocalDateTime.now());
                    }
                    post.setUpdatedAt(LocalDateTime.now());
                    return PostMapper.toDTO(postRepository.save(post));
                }
            }
        }
        Optional.ofNullable(status).ifPresent(post::setStatus);
        Optional.ofNullable(deleted).ifPresent(post::setDeleted);
        if(Boolean.TRUE.equals(deleted)){
            post.setDeletedAt(LocalDateTime.now());
        }
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
        return changePostStatus(userId,postId,Status.ACTIVE,false);
    }
    @CachePut(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO hiddenPost(Long userId,Long postId){
        return changePostStatus(userId,postId,Status.HIDDEN,false);
    }
    @CachePut(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO reportedPost(Long userId,Long postId){
        return changePostStatus(userId,postId,Status.REPORTED,false);
    }
    @CachePut(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO underReviewPost(Long userId,Long postId){
        return changePostStatus(userId,postId,Status.UNDER_REVIEW,false);
    }
    @CacheEvict(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO rejectedPost(Long userId,Long postId){
        return changePostStatus(userId,postId,Status.REJECTED,true);
    }
    @CacheEvict(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO removeByAdminPost(Long userId,Long postId){
        return changePostStatus(userId,postId,Status.REMOVED_BY_ADMIN,true);
    }
    @CachePut(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO pendingApprovalPost(Long userId,Long postId){
        return changePostStatus(userId,postId,Status.PENDING_APPROVAL,false);
    }
    @CacheEvict(value = "postsByUserAndPostIDs",key = "#userId + '-' + #postId")
    public PostDTO deletedPost(Long userId,Long postId){
        return changePostStatus(userId,postId,Status.DELETED,true);
    }

    public void savePost(Post post){
        postRepository.save(post);
    }
    public Page<PostDTO> getAllPosts(Pageable pageable){
        return postRepository.findAll(pageable).map(PostMapper::toDTO);
    }
}
