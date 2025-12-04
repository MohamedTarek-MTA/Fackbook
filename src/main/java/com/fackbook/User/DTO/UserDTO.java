package com.fackbook.User.DTO;

import com.fackbook.User.Enum.Gender;
import com.fackbook.User.Enum.Role;
import com.fackbook.User.Enum.Status;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserDTO implements Serializable {

    private Long id;

    private String name;
    private String email;
    private String phone;
    private String age;
    private String address;
    private String bio;
    private String imageUrl;

    private LocalDate birthdate;

    private Gender gender;
    private Role role;
    private Status status;

    private Boolean enabled;
    private Boolean deleted;
    private Boolean removeImage;

    private LocalDateTime lastLoginDate;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;


}
