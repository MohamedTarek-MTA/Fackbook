package com.fackbook.Reply.DTO;

import com.fackbook.Post.Enum.Status;
import lombok.*;

import java.io.Serializable;
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

    private Status status;

    private LocalDateTime createdAt;
    private LocalDateTime deletedAt;
    private LocalDateTime updatedAt;

    private Boolean deleted;
}
