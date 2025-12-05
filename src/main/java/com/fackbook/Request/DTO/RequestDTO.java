package com.fackbook.Request.DTO;

import com.fackbook.Request.Enum.RequestActionType;
import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.Request.Enum.Status;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDateTime;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class RequestDTO implements Serializable {

    private Long id;

    private Long userId;
    private Long targetId;

    @NotNull
    private RequestTargetType targetType;
    @NotNull
    private RequestActionType actionType;
    private Status status;

    private LocalDateTime createdAt;
    private LocalDateTime deletedAt;
    private LocalDateTime updatedAt;

    private Boolean deleted;

}
