package com.fackbook.Post.DTO;

import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Enum.Privacy;
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
public class PostDTO implements Serializable {

    private Long id;


    private Long userId;
    private Long groupId;


    private String content;
    private String imageUrl;
    private String videoUrl;

    private BigInteger numberOfReacts;
    private BigInteger numberOfComments;
    private BigInteger numberOfShares;


    private Privacy privacy;

    private VisibilityStatus visibilityStatus;
    private ModerationStatus moderationStatus;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;

    private Boolean deleted;
    private Boolean removeImage;
    private Boolean removeVideo;
}
