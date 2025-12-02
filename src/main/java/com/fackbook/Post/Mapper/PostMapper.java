package com.fackbook.Post.Mapper;

import com.fackbook.Post.DTO.PostDTO;
import com.fackbook.Post.Entity.Post;
import org.springframework.stereotype.Component;

@Component
public class PostMapper {

    public static PostDTO toDTO(Post post) {
        if (post == null) {
            return null;
        }

        return PostDTO.builder()
                .id(post.getId())
                .userId(post.getUser() != null ? post.getUser().getId() : null)
                .groupId(post.getGroup() != null ? post.getGroup().getId() : null)
                .content(post.getContent())
                .imageUrl(post.getImageUrl())
                .videoUrl(post.getVideoUrl())
                .numberOfReacts(post.getNumberOfReacts())
                .numberOfComments(post.getNumberOfComments())
                .numberOfShares(post.getNumberOfShares())
                .privacy(post.getPrivacy())
                .visibilityStatus(post.getVisibilityStatus())
                .moderationStatus(post.getModerationStatus())
                .createdAt(post.getCreatedAt())
                .updatedAt(post.getUpdatedAt())
                .deletedAt(post.getDeletedAt())
                .deleted(post.getDeleted())
                .build();
    }

    public static Post toEntity(PostDTO postDTO) {
        if (postDTO == null) {
            return null;
        }

        return Post.builder()
                .id(postDTO.getId())
                .content(postDTO.getContent())
                .imageUrl(postDTO.getImageUrl())
                .videoUrl(postDTO.getVideoUrl())
                .numberOfReacts(postDTO.getNumberOfReacts())
                .numberOfComments(postDTO.getNumberOfComments())
                .numberOfShares(postDTO.getNumberOfShares())
                .privacy(postDTO.getPrivacy())
                .visibilityStatus(postDTO.getVisibilityStatus())
                .moderationStatus(postDTO.getModerationStatus())
                .createdAt(postDTO.getCreatedAt())
                .updatedAt(postDTO.getUpdatedAt())
                .deletedAt(postDTO.getDeletedAt())
                .deleted(postDTO.getDeleted())
                .build();
    }
}

