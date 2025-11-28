package com.fackbook.Share.DTO;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDateTime;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class ShareDTO implements Serializable {

    private Long id;

    @NotNull
    private Long userId;
    @NotNull
    private Long postId;

    @NotBlank
    private String content;

    private LocalDateTime createdAt;
}
