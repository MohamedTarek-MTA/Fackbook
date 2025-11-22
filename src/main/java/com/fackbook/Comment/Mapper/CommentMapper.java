package com.fackbook.Comment.Mapper;

import com.fackbook.Comment.DTO.CommentDTO;
import com.fackbook.Comment.Entity.Comment;
import org.springframework.stereotype.Component;

@Component
public class CommentMapper {

    public static CommentDTO toDTO(Comment comment) {
        if (comment == null) {
            return null;
        }

        return CommentDTO.builder()
                .id(comment.getId())
                .userId(comment.getUser() != null ? comment.getUser().getId() : null)
                .postId(comment.getPost() != null ? comment.getPost().getId() : null)
                .content(comment.getContent())
                .imageUrl(comment.getImageUrl())
                .videoUrl(comment.getVideoUrl())
                .status(comment.getStatus())
                .createdAt(comment.getCreatedAt())
                .updatedAt(comment.getUpdatedAt())
                .deletedAt(comment.getDeletedAt())
                .deleted(comment.getDeleted())
                .build();
    }

    public static Comment toEntity(CommentDTO commentDTO) {
        if (commentDTO == null) {
            return null;
        }

        return Comment.builder()
                .id(commentDTO.getId())
                .content(commentDTO.getContent())
                .imageUrl(commentDTO.getImageUrl())
                .videoUrl(commentDTO.getVideoUrl())
                .status(commentDTO.getStatus())
                .createdAt(commentDTO.getCreatedAt())
                .updatedAt(commentDTO.getUpdatedAt())
                .deletedAt(commentDTO.getDeletedAt())
                .deleted(commentDTO.getDeleted())
                .build();
    }
}

