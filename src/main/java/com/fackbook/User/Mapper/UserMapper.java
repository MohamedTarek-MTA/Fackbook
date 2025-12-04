package com.fackbook.User.Mapper;

import com.fackbook.Shared.Helper.Helper;
import com.fackbook.User.DTO.UserDTO;
import com.fackbook.User.Entity.User;
import org.springframework.stereotype.Component;

@Component
public class UserMapper {

    public static UserDTO toDTO(User user) {
        if (user == null) {
            return null;
        }

        return UserDTO.builder()
                .id(user.getId())
                .name(user.getName())
                .email(user.getEmail())
                .phone(user.getPhone())
                .address(user.getAddress())
                .bio(user.getBio())
                .age(Helper.getAge(user.getBirthdate()))
                .imageUrl(user.getImageUrl())
                .removeImage(null)
                .birthdate(user.getBirthdate())
                .gender(user.getGender())
                .role(user.getRole())
                .status(user.getStatus())
                .enabled(user.getEnabled())
                .deleted(user.getDeleted())
                .lastLoginDate(user.getLastLoginDate())
                .createdAt(user.getCreatedAt())
                .updatedAt(user.getUpdatedAt())
                .deletedAt(user.getDeletedAt())
                .build();
    }

    public static User toEntity(UserDTO userDTO) {
        if (userDTO == null) {
            return null;
        }

        return User.builder()
                .id(userDTO.getId())
                .name(userDTO.getName())
                .email(userDTO.getEmail())
                .phone(userDTO.getPhone())
                .address(userDTO.getAddress())
                .bio(userDTO.getBio())
                .imageUrl(userDTO.getImageUrl())
                .birthdate(userDTO.getBirthdate())
                .gender(userDTO.getGender())
                .role(userDTO.getRole())
                .status(userDTO.getStatus())
                .enabled(userDTO.getEnabled())
                .deleted(userDTO.getDeleted())
                .lastLoginDate(userDTO.getLastLoginDate())
                .createdAt(userDTO.getCreatedAt())
                .updatedAt(userDTO.getUpdatedAt())
                .deletedAt(userDTO.getDeletedAt())
                .build();
    }
}

