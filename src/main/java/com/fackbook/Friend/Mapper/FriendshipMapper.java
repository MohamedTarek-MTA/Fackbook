package com.fackbook.Friend.Mapper;

import com.fackbook.Friend.DTO.FriendshipDTO;
import com.fackbook.Friend.Entity.Friendship;
import org.springframework.stereotype.Component;

@Component
public class FriendshipMapper {

    public static FriendshipDTO toDTO(Friendship friendship) {
        if (friendship == null) {
            return null;
        }

        return FriendshipDTO.builder()
                .id(friendship.getId())
                .userId(friendship.getUser() != null ? friendship.getUser().getId() : null)
                .friendId(friendship.getFriend() != null ? friendship.getFriend().getId() : null)
                .status(friendship.getStatus())
                .createdAt(friendship.getCreatedAt())
                .deletedAt(friendship.getDeletedAt())
                .updatedAt(friendship.getUpdatedAt())
                .deleted(friendship.getDeleted())
                .build();
    }

    public static Friendship toEntity(FriendshipDTO friendshipDTO) {
        if (friendshipDTO == null) {
            return null;
        }

        return Friendship.builder()
                .id(friendshipDTO.getId())
                .status(friendshipDTO.getStatus())
                .createdAt(friendshipDTO.getCreatedAt())
                .deletedAt(friendshipDTO.getDeletedAt())
                .updatedAt(friendshipDTO.getUpdatedAt())
                .deleted(friendshipDTO.getDeleted())
                .build();
    }
}

