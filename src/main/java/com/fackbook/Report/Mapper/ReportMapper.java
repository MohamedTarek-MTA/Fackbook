package com.fackbook.Report.Mapper;

import com.fackbook.Report.DTO.ReportDTO;
import com.fackbook.Report.Entity.Report;
import org.springframework.stereotype.Component;

@Component
public class ReportMapper {

    public static ReportDTO toDTO(Report report) {
        if (report == null) {
            return null;
        }

        return ReportDTO.builder()
                .id(report.getId())
                .userId(report.getUser() != null ? report.getUser().getId() : null)
                .targetId(report.getTargetId())
                .content(report.getContent())
                .reviewerId(report.getReviewer()!= null? report.getReviewer().getId() : null)
                .imageUrl(report.getImageUrl())
                .videoUrl(report.getVideoUrl())
                .status(report.getStatus())
                .targetType(report.getTargetType())
                .createdAt(report.getCreatedAt())
                .updatedAt(report.getUpdatedAt())
                .deletedAt(report.getDeletedAt())
                .deleted(report.getDeleted())
                .build();
    }

    public static Report toEntity(ReportDTO reportDTO) {
        if (reportDTO == null) {
            return null;
        }

        return Report.builder()
                .id(reportDTO.getId())
                .targetId(reportDTO.getTargetId())
                .content(reportDTO.getContent())
                .imageUrl(reportDTO.getImageUrl())
                .videoUrl(reportDTO.getVideoUrl())
                .status(reportDTO.getStatus())
                .targetType(reportDTO.getTargetType())
                .createdAt(reportDTO.getCreatedAt())
                .updatedAt(reportDTO.getUpdatedAt())
                .deletedAt(reportDTO.getDeletedAt())
                .deleted(reportDTO.getDeleted())
                .build();
    }
}

