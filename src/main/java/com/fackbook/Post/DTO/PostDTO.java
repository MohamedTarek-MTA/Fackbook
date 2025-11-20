package com.fackbook.Post.DTO;

import com.fackbook.Post.Enum.Privacy;
import com.fackbook.Post.Enum.Status;
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
    private Status status;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;

    private Boolean deleted;
}
