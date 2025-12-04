package com.fackbook.Reply.Mapper;

import com.fackbook.Reply.DTO.ReplyDTO;
import com.fackbook.Reply.Entity.Reply;
import org.springframework.stereotype.Component;

@Component
public class ReplyMapper {

    public static ReplyDTO toDTO(Reply reply) {
        if (reply == null) {
            return null;
        }

        return ReplyDTO.builder()
                .id(reply.getId())
                .userId(reply.getUser() != null ? reply.getUser().getId() : null)
                .commentId(reply.getComment() != null ? reply.getComment().getId() : null)
                .content(reply.getContent())
                .imageUrl(reply.getImageUrl())
                .videoUrl(reply.getVideoUrl())
                .visibilityStatus(reply.getVisibilityStatus())
                .moderationStatus(reply.getModerationStatus())
                .createdAt(reply.getCreatedAt())
                .deletedAt(reply.getDeletedAt())
                .updatedAt(reply.getUpdatedAt())
                .deleted(reply.getDeleted())
                .removeImage(null)
                .removeVideo(null)
                .build();
    }

    public static Reply toEntity(ReplyDTO replyDTO) {
        if (replyDTO == null) {
            return null;
        }

        return Reply.builder()
                .id(replyDTO.getId())
                .content(replyDTO.getContent())
                .imageUrl(replyDTO.getImageUrl())
                .videoUrl(replyDTO.getVideoUrl())
                .visibilityStatus(replyDTO.getVisibilityStatus())
                .moderationStatus(replyDTO.getModerationStatus())
                .createdAt(replyDTO.getCreatedAt())
                .deletedAt(replyDTO.getDeletedAt())
                .updatedAt(replyDTO.getUpdatedAt())
                .deleted(replyDTO.getDeleted())
                .build();
    }
}

