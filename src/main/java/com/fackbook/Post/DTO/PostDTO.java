package com.fackbook.Post.DTO;

import com.fackbook.Post.Enum.Privacy;
import com.fackbook.Post.Enum.Status;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
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

    @NotNull
    private Long userId;
    private Long groupId;

    @NotBlank
    private String content;
    private String imageUrl;
    private String videoUrl;

    private BigInteger numberOfReacts;
    private BigInteger numberOfComments;
    private BigInteger numberOfShares;

    @NotNull
    private Privacy privacy;
    @NotNull
    private Status status;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;

    private Boolean deleted;
}
