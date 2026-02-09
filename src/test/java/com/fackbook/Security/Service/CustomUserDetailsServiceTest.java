package com.fackbook.Security.Service;

import com.fackbook.TestFixtures;
import com.fackbook.User.Entity.User;
import com.fackbook.User.Repository.UserRepository;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("CustomUserDetailsService tests")
class CustomUserDetailsServiceTest {

    @Mock
    private UserRepository userRepository;

    @InjectMocks
    private CustomUserDetailsService customUserDetailsService;

    @Nested
    @DisplayName("loadUserByUsername")
    class LoadUserByUsername {
        @Test
        void returnsUserDetailsWhenUserFound() {
            User user = TestFixtures.user(1L);
            when(userRepository.findByEmail("user@test.com")).thenReturn(Optional.of(user));

            UserDetails result = customUserDetailsService.loadUserByUsername("user@test.com");

            assertThat(result).isNotNull();
            assertThat(result.getUsername()).isEqualTo("user@test.com");
        }

        @Test
        void throwsUsernameNotFoundExceptionWhenUserNotFound() {
            when(userRepository.findByEmail("missing@test.com")).thenReturn(Optional.empty());

            assertThatThrownBy(() -> customUserDetailsService.loadUserByUsername("missing@test.com"))
                    .isInstanceOf(UsernameNotFoundException.class)
                    .hasMessageContaining("User Email Not Found");
        }
    }
}
