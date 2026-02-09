package com.fackbook.User.Service;

import com.fackbook.TestFixtures;
import com.fackbook.User.DTO.UserDTO;
import com.fackbook.User.Entity.User;
import com.fackbook.User.Enum.Gender;
import com.fackbook.User.Enum.Role;
import com.fackbook.User.Enum.Status;
import com.fackbook.User.Repository.UserRepository;
import com.fackbook.Post.Util.Service.MediaManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("UserService tests")
class UserServiceTest {

    @Mock
    private UserRepository userRepository;

    @Mock
    private MediaManager mediaManager;

    @InjectMocks
    private UserService userService;

    private User user;
    private Pageable pageable;

    @BeforeEach
    void setUp() {
        user = TestFixtures.user(1L);
        pageable = TestFixtures.pageable();
    }

    @Nested
    @DisplayName("userExistsByEmail")
    class UserExistsByEmail {
        @Test
        void returnsTrueWhenEmailExists() {
            when(userRepository.findByEmail("user@test.com")).thenReturn(Optional.of(user));
            assertThat(userService.userExistsByEmail("user@test.com")).isTrue();
        }

        @Test
        void returnsFalseWhenEmailNotExists() {
            when(userRepository.findByEmail("other@test.com")).thenReturn(Optional.empty());
            assertThat(userService.userExistsByEmail("other@test.com")).isFalse();
        }
    }

    @Nested
    @DisplayName("userExistsByPhone")
    class UserExistsByPhone {
        @Test
        void returnsTrueWhenPhoneExists() {
            when(userRepository.findByPhone("+1234567890")).thenReturn(Optional.of(user));
            assertThat(userService.userExistsByPhone("+1234567890")).isTrue();
        }

        @Test
        void returnsFalseWhenPhoneNotExists() {
            when(userRepository.findByPhone("+9999999999")).thenReturn(Optional.empty());
            assertThat(userService.userExistsByPhone("+9999999999")).isFalse();
        }
    }

    @Nested
    @DisplayName("getUserById")
    class GetUserById {
        @Test
        void returnsUserDTOWhenFound() {
            when(userRepository.findById(1L)).thenReturn(Optional.of(user));
            UserDTO result = userService.getUserById(1L);
            assertThat(result).isNotNull();
            assertThat(result.getId()).isEqualTo(1L);
            assertThat(result.getEmail()).isEqualTo("user@test.com");
        }

        @Test
        void throwsWhenNotFound() {
            when(userRepository.findById(999L)).thenReturn(Optional.empty());
            assertThatThrownBy(() -> userService.getUserById(999L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("User Not Found");
        }
    }

    @Nested
    @DisplayName("getUserEntityById")
    class GetUserEntityById {
        @Test
        void returnsUserWhenFound() {
            when(userRepository.findById(1L)).thenReturn(Optional.of(user));
            User result = userService.getUserEntityById(1L);
            assertThat(result).isSameAs(user);
        }

        @Test
        void throwsWhenNotFound() {
            when(userRepository.findById(999L)).thenReturn(Optional.empty());
            assertThatThrownBy(() -> userService.getUserEntityById(999L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("User Not Found");
        }
    }

    @Nested
    @DisplayName("getUserByEmail")
    class GetUserByEmail {
        @Test
        void returnsUserDTOWhenFound() {
            when(userRepository.findByEmail("user@test.com")).thenReturn(Optional.of(user));
            UserDTO result = userService.getUserByEmail("user@test.com");
            assertThat(result).isNotNull();
            assertThat(result.getEmail()).isEqualTo("user@test.com");
        }

        @Test
        void throwsWhenNotFound() {
            when(userRepository.findByEmail("x@test.com")).thenReturn(Optional.empty());
            assertThatThrownBy(() -> userService.getUserByEmail("x@test.com"))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Not Found");
        }
    }

    @Nested
    @DisplayName("getUserByPhone")
    class GetUserByPhone {
        @Test
        void returnsUserDTOWhenFound() {
            when(userRepository.findByPhone("+1234567890")).thenReturn(Optional.of(user));
            UserDTO result = userService.getUserByPhone("+1234567890");
            assertThat(result).isNotNull();
            assertThat(result.getPhone()).isEqualTo("+1234567890");
        }

        @Test
        void throwsWhenNotFound() {
            when(userRepository.findByPhone("+0")).thenReturn(Optional.empty());
            assertThatThrownBy(() -> userService.getUserByPhone("+0"))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Not Found");
        }
    }

    @Nested
    @DisplayName("getUserEntityByEmail")
    class GetUserEntityByEmail {
        @Test
        void returnsUserWhenFound() {
            when(userRepository.findByEmail("user@test.com")).thenReturn(Optional.of(user));
            User result = userService.getUserEntityByEmail("user@test.com");
            assertThat(result).isSameAs(user);
        }

        @Test
        void throwsWhenNotFound() {
            when(userRepository.findByEmail("x@test.com")).thenReturn(Optional.empty());
            assertThatThrownBy(() -> userService.getUserEntityByEmail("x@test.com"))
                    .isInstanceOf(IllegalArgumentException.class);
        }
    }

    @Nested
    @DisplayName("updateLastLoginDateById")
    class UpdateLastLoginDateById {
        @Test
        void updatesAndSavesUser() {
            when(userRepository.findById(1L)).thenReturn(Optional.of(user));
            userService.updateLastLoginDateById(1L);
            verify(userRepository).save(user);
            assertThat(user.getLastLoginDate()).isNotNull();
        }
    }

    @Nested
    @DisplayName("getMaleUsers / getFemaleUsers")
    class GetUsersByGender {
        @Test
        void getMaleUsersReturnsPage() {
            when(userRepository.findByGender(Gender.MALE, pageable))
                    .thenReturn(new PageImpl<>(List.of(user)));
            Page<UserDTO> result = userService.getMaleUsers(pageable);
            assertThat(result).isNotEmpty();
        }

        @Test
        void getFemaleUsersReturnsPage() {
            when(userRepository.findByGender(Gender.FEMALE, pageable))
                    .thenReturn(new PageImpl<>(List.of()));
            Page<UserDTO> result = userService.getFemaleUsers(pageable);
            assertThat(result).isEmpty();
        }
    }

    @Nested
    @DisplayName("getActiveUsers / getInactiveUsers")
    class GetUsersByStatus {
        @Test
        void getActiveUsersReturnsPage() {
            when(userRepository.findByStatus(Status.ACTIVE, pageable))
                    .thenReturn(new PageImpl<>(List.of(user)));
            Page<UserDTO> result = userService.getActiveUsers(pageable);
            assertThat(result).isNotEmpty();
        }

        @Test
        void getInactiveUsersReturnsPage() {
            when(userRepository.findByStatus(Status.INACTIVE, pageable))
                    .thenReturn(new PageImpl<>(List.of()));
            Page<UserDTO> result = userService.getInactiveUsers(pageable);
            assertThat(result).isEmpty();
        }
    }

    @Nested
    @DisplayName("getUsersByName / getUsersByAddress")
    class GetUsersBySearch {
        @Test
        void getUsersByNameReturnsPage() {
            when(userRepository.findByNameContainingIgnoreCase("Test", pageable))
                    .thenReturn(new PageImpl<>(List.of(user)));
            Page<UserDTO> result = userService.getUsersByName("Test", pageable);
            assertThat(result).isNotEmpty();
        }

        @Test
        void getUsersByAddressReturnsPage() {
            when(userRepository.findByAddressContainingIgnoreCase("Cairo", pageable))
                    .thenReturn(new PageImpl<>(List.of()));
            Page<UserDTO> result = userService.getUsersByAddress("Cairo", pageable);
            assertThat(result).isEmpty();
        }
    }

    @Nested
    @DisplayName("activeUser / inactiveUser / banUser / deleteUser")
    class ChangeUserStatus {
        @Test
        void activeUserUpdatesStatusAndSaves() {
            user.setStatus(Status.INACTIVE);
            when(userRepository.findById(1L)).thenReturn(Optional.of(user));
            when(userRepository.save(any(User.class))).thenAnswer(i -> i.getArgument(0));
            UserDTO result = userService.activeUser(1L);
            verify(userRepository).save(user);
            assertThat(result).isNotNull();
        }

        @Test
        void deleteUserSetsDeletedAndSaves() {
            when(userRepository.findById(1L)).thenReturn(Optional.of(user));
            when(userRepository.save(any(User.class))).thenAnswer(i -> i.getArgument(0));
            UserDTO result = userService.deleteUser(1L);
            verify(userRepository).save(user);
            assertThat(user.getDeleted()).isTrue();
            assertThat(result).isNotNull();
        }
    }

    @Nested
    @DisplayName("toSystemAdmin / toNormalUser")
    class ChangeUserRole {
        @Test
        void toSystemAdminUpdatesRoleAndSaves() {
            when(userRepository.findById(1L)).thenReturn(Optional.of(user));
            when(userRepository.save(any(User.class))).thenAnswer(i -> i.getArgument(0));
            UserDTO result = userService.toSystemAdmin(1L);
            verify(userRepository).save(user);
            assertThat(result).isNotNull();
        }
    }

    @Nested
    @DisplayName("saveUser")
    class SaveUser {
        @Test
        void savesUser() {
            userService.saveUser(user);
            verify(userRepository).save(user);
        }
    }

    @Nested
    @DisplayName("getAllUsers")
    class GetAllUsers {
        @Test
        void returnsPageOfUsers() {
            when(userRepository.findAll(pageable)).thenReturn(new PageImpl<>(List.of(user)));
            Page<UserDTO> result = userService.getAllUsers(pageable);
            assertThat(result).isNotEmpty();
        }
    }
}
