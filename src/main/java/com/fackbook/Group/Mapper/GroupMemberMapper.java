package com.fackbook.Group.Mapper;

import com.fackbook.Group.DTO.GroupMemberDTO;
import com.fackbook.Group.Entity.GroupMember;
import org.springframework.stereotype.Component;

@Component
public class GroupMemberMapper {

    public static GroupMemberDTO toDTO(GroupMember groupMember) {
        if (groupMember == null) {
            return null;
        }

        return GroupMemberDTO.builder()
                .id(groupMember.getId())
                .userId(groupMember.getUser() != null ? groupMember.getUser().getId() : null)
                .groupId(groupMember.getGroup() != null ? groupMember.getGroup().getId() : null)
                .role(groupMember.getRole())
                .status(groupMember.getStatus())
                .createdAt(groupMember.getCreatedAt())
                .deletedAt(groupMember.getDeletedAt())
                .updatedAt(groupMember.getUpdatedAt())
                .deleted(groupMember.getDeleted())
                .build();
    }

    public static GroupMember toEntity(GroupMemberDTO groupMemberDTO) {
        if (groupMemberDTO == null) {
            return null;
        }

        return GroupMember.builder()
                .id(groupMemberDTO.getId())
                .role(groupMemberDTO.getRole())
                .status(groupMemberDTO.getStatus())
                .createdAt(groupMemberDTO.getCreatedAt())
                .deletedAt(groupMemberDTO.getDeletedAt())
                .updatedAt(groupMemberDTO.getUpdatedAt())
                .deleted(groupMemberDTO.getDeleted())
                .build();
    }
}

