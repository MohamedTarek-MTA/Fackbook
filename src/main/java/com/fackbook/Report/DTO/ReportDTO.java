package com.fackbook.Report.DTO;

import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.Request.Enum.Status;
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
    private RequestTargetType targetType;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;

    private Boolean deleted;
}
