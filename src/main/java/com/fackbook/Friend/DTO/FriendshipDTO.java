package com.fackbook.Friend.DTO;

import com.fackbook.Friend.Enum.Status;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class FriendshipDTO implements Serializable {

    private Long id;

    @NotNull
    private Long userId;
    @NotNull
    private Long friendId;

    private Status status;

    private LocalDateTime createdAt;
    private LocalDateTime deletedAt;
    private LocalDateTime updatedAt;

    private Boolean deleted;
}
