package com.fackbook.React.DTO;

import com.fackbook.React.Enum.ReactType;
import com.fackbook.Request.Enum.TargetType;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ReactDTO implements Serializable {

    private Long id;

    private Long userId;
    private Long targetId;

    private TargetType targetType;
    private ReactType reactType;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;

    private Boolean deleted;
}
