package com.fackbook.Group.Mapper;

import com.fackbook.Group.DTO.GroupDTO;
import com.fackbook.Group.Entity.Group;
import org.springframework.stereotype.Component;

@Component
public class GroupMapper {

    public static GroupDTO toDTO(Group group) {
        if (group == null) {
            return null;
        }

        return GroupDTO.builder()
                .id(group.getId())
                .userId(group.getUser() != null ? group.getUser().getId() : null)
                .name(group.getName())
                .description(group.getDescription())
                .imageUrl(group.getImageUrl())
                .numberOfMembers(group.getNumberOfMembers())
                .status(group.getStatus())
                .approvalMode(group.getApprovalMode())
                .joinPolicy(group.getJoinPolicy())
                .createdAt(group.getCreatedAt())
                .deletedAt(group.getDeletedAt())
                .updatedAt(group.getUpdatedAt())
                .deleted(group.getDeleted())
                .build();
    }

    public static Group toEntity(GroupDTO groupDTO) {
        if (groupDTO == null) {
            return null;
        }

        return Group.builder()
                .id(groupDTO.getId())
                .name(groupDTO.getName())
                .description(groupDTO.getDescription())
                .imageUrl(groupDTO.getImageUrl())
                .numberOfMembers(groupDTO.getNumberOfMembers())
                .status(groupDTO.getStatus())
                .approvalMode(groupDTO.getApprovalMode())
                .joinPolicy(groupDTO.getJoinPolicy())
                .createdAt(groupDTO.getCreatedAt())
                .deletedAt(groupDTO.getDeletedAt())
                .updatedAt(groupDTO.getUpdatedAt())
                .deleted(groupDTO.getDeleted())
                .build();
    }
}

