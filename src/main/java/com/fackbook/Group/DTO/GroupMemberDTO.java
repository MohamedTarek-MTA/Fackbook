package com.fackbook.Group.DTO;

import com.fackbook.User.Enum.Role;
import com.fackbook.User.Enum.Status;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDateTime;


@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class GroupMemberDTO implements Serializable {

    private Long id;

    @NotNull
    private Long userId;
    @NotNull
    private Long groupId;

    private Role role;
    private Status status;

    private LocalDateTime createdAt;
    private LocalDateTime deletedAt;
    private LocalDateTime updatedAt;

    private Boolean deleted;
}
