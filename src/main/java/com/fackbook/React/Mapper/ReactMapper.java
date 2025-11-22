package com.fackbook.React.Mapper;

import com.fackbook.React.DTO.ReactDTO;
import com.fackbook.React.Entity.React;
import org.springframework.stereotype.Component;

@Component
public class ReactMapper {

    public static ReactDTO toDTO(React react) {
        if (react == null) {
            return null;
        }

        return ReactDTO.builder()
                .id(react.getId())
                .userId(react.getUser() != null ? react.getUser().getId() : null)
                .targetId(react.getTargetId())
                .targetType(react.getTargetType())
                .reactType(react.getReactType())
                .createdAt(react.getCreatedAt())
                .updatedAt(react.getUpdatedAt())
                .deletedAt(react.getDeletedAt())
                .deleted(react.getDeleted())
                .build();
    }

    public static React toEntity(ReactDTO reactDTO) {
        if (reactDTO == null) {
            return null;
        }

        return React.builder()
                .id(reactDTO.getId())
                .targetId(reactDTO.getTargetId())
                .targetType(reactDTO.getTargetType())
                .reactType(reactDTO.getReactType())
                .createdAt(reactDTO.getCreatedAt())
                .updatedAt(reactDTO.getUpdatedAt())
                .deletedAt(reactDTO.getDeletedAt())
                .deleted(reactDTO.getDeleted())
                .build();
    }
}

