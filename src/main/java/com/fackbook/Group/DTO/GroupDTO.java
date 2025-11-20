package com.fackbook.Group.DTO;

import com.fackbook.User.Enum.Status;
import lombok.*;

import java.io.Serializable;
import java.math.BigInteger;
import java.time.LocalDateTime;


@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class GroupDTO implements Serializable {

    private Long id;

    private Long userId;

    private String name;
    private String description;
    private String imageUrl;

    private BigInteger numberOfMembers;

    private Status status;

    private LocalDateTime createdAt;
    private LocalDateTime deletedAt;
    private LocalDateTime updatedAt;

    private Boolean deleted;
}
