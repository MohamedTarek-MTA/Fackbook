package com.fackbook.Comment.DTO;

import com.fackbook.Post.Enum.Status;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CommentDTO implements Serializable {

    private Long id;

    private Long userId;
    private Long postId;

    private String content;
    private String imageUrl;
    private String videoUrl;

    private Status status;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;

    private Boolean deleted;
}
