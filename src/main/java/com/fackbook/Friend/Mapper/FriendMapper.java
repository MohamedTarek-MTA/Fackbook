package com.fackbook.Friend.Mapper;

import com.fackbook.Friend.DTO.FriendDTO;
import com.fackbook.Friend.Entity.Friend;
import org.springframework.stereotype.Component;

@Component
public class FriendMapper {

    public static FriendDTO toDTO(Friend friend) {
        if (friend == null) {
            return null;
        }

        return FriendDTO.builder()
                .id(friend.getId())
                .userId(friend.getUser() != null ? friend.getUser().getId() : null)
                .friendId(friend.getFriend() != null ? friend.getFriend().getId() : null)
                .status(friend.getStatus())
                .createdAt(friend.getCreatedAt())
                .deletedAt(friend.getDeletedAt())
                .updatedAt(friend.getUpdatedAt())
                .deleted(friend.getDeleted())
                .build();
    }

    public static Friend toEntity(FriendDTO friendDTO) {
        if (friendDTO == null) {
            return null;
        }

        return Friend.builder()
                .id(friendDTO.getId())
                .status(friendDTO.getStatus())
                .createdAt(friendDTO.getCreatedAt())
                .deletedAt(friendDTO.getDeletedAt())
                .updatedAt(friendDTO.getUpdatedAt())
                .deleted(friendDTO.getDeleted())
                .build();
    }
}

