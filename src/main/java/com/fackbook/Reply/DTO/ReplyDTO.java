package com.fackbook.Reply.DTO;

import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Enum.VisibilityStatus;
import lombok.*;

import java.io.Serializable;
import java.math.BigInteger;
import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ReplyDTO implements Serializable {

    private Long id;

    private Long userId;
    private Long commentId;

    private String content;
    private String imageUrl;
    private String videoUrl;

    private VisibilityStatus visibilityStatus;
    private ModerationStatus moderationStatus;

    private BigInteger numberOfReacts;

    private LocalDateTime createdAt;
    private LocalDateTime deletedAt;
    private LocalDateTime updatedAt;

    private Boolean deleted;
    private Boolean removeImage;
    private Boolean removeVideo;
}
