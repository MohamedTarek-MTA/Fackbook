package com.fackbook.Security.DTO;

import com.fackbook.User.Enum.Gender;
import com.fackbook.User.Enum.Role;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import lombok.*;

import java.io.Serializable;
import java.time.LocalDate;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RegisterRequest implements Serializable {
    @NotBlank(message = "First Name is required !")
    private String firstName;
    @NotBlank(message = "Last Name is required !")
    private String lastName;
    @NotBlank(message = "First Name is required !")
    @Email(message = "Please insert valid email !")
    private String email;
    @NotBlank(message = "Password is required !")
    @Pattern(
            regexp = "^(?=.*[a-z])" +
            "(?=.*[A-Z])" +
            "(?=.*\\d)" +
            "(?=.*[@$!%*?&])" +
            "[A-Za-z\\d@$!%*?&]{8,}$",
            message = "Password must be at least 8 characters long and contain at least one uppercase letter, one lowercase letter, one digit, and one special character."
    )
    private String password;
    @NotBlank(message = "Phone Number is required !")
    @Pattern(regexp = "^\\+?[0-9]{10,15}$", message = "Please insert valid phone number !")
    private String phone;
    private String address;

    @NotNull(message = "Gender is required !")
    private Gender gender;
    @NotNull(message = "Role is required !")
    private Role role;

    @NotNull(message = "Birthdate is required !")
    private LocalDate birthdate;

}
