package com.fackbook.Share.DTO;

import lombok.*;

import java.io.Serializable;
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class ShareDTO implements Serializable {

    private Long id;

    private Long userId;
    private Long postId;

    private String content;
}
