package com.fackbook.Request.Mapper;

import com.fackbook.Request.DTO.RequestDTO;
import com.fackbook.Request.Entity.Request;
import org.springframework.stereotype.Component;

@Component
public class RequestMapper {

    public static RequestDTO toDTO(Request request) {
        if (request == null) {
            return null;
        }

        return RequestDTO.builder()
                .id(request.getId())
                .userId(request.getUser() != null ? request.getUser().getId() : null)
                .targetId(request.getTargetId())
                .targetType(request.getTargetType())
                .status(request.getStatus())
                .createdAt(request.getCreatedAt())
                .deletedAt(request.getDeletedAt())
                .updatedAt(request.getUpdatedAt())
                .deleted(request.getDeleted())
                .build();
    }

    public static Request toEntity(RequestDTO requestDTO) {
        if (requestDTO == null) {
            return null;
        }

        return Request.builder()
                .id(requestDTO.getId())
                .targetId(requestDTO.getTargetId())
                .targetType(requestDTO.getTargetType())
                .status(requestDTO.getStatus())
                .createdAt(requestDTO.getCreatedAt())
                .deletedAt(requestDTO.getDeletedAt())
                .updatedAt(requestDTO.getUpdatedAt())
                .deleted(requestDTO.getDeleted())
                .build();
    }
}

