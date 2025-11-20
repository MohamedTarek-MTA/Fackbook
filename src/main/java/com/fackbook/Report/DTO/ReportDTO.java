package com.fackbook.Report.DTO;

import com.fackbook.Request.Enum.Status;
import com.fackbook.Request.Enum.TargetType;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ReportDTO implements Serializable {

    private Long id;

    private Long userId;
    private Long targetId;

    private String content;
    private String imageUrl;
    private String videoUrl;

    private Status status;
    private TargetType targetType;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;

    private Boolean deleted;
}
