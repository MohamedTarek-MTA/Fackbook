package com.fackbook.Request.DTO;

import com.fackbook.Request.Enum.Status;
import com.fackbook.Request.Enum.TargetType;
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

    private TargetType targetType;
    private Status status;

    private LocalDateTime createdAt;
    private LocalDateTime deletedAt;
    private LocalDateTime updatedAt;

    private Boolean deleted;

}
