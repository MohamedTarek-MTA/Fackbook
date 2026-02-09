package com.fackbook.Post.Util.Service;

import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Enum.VisibilityStatus;
import com.fackbook.Post.Util.Interface.AccessibleContent;
import com.fackbook.TestFixtures;
import com.fackbook.User.Entity.User;
import com.fackbook.User.Enum.Role;
import com.fackbook.User.Service.UserService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("AccessibilityService tests")
class AccessibilityServiceTest {

    @Mock
    private UserService userService;

    @InjectMocks
    private AccessibilityService accessibilityService;

    private User user;
    private AccessibleContent content;

    @BeforeEach
    void setUp() {
        user = TestFixtures.user(1L);
        content = new AccessibleContent() {
            @Override
            public Long getAuthorId() { return 1L; }
            @Override
            public Long getPostAuthorId() { return 1L; }
            @Override
            public Long getGroupOwnerId() { return null; }
            @Override
            public VisibilityStatus getVisibilityStatus() { return VisibilityStatus.ACTIVE; }
            @Override
            public ModerationStatus getModerationStatus() { return ModerationStatus.NONE; }
        };
    }

    @Nested
    @DisplayName("validateVisibility")
    class ValidateVisibility {
        @Test
        void doesNotThrowWhenActive() {
            when(userService.getUserEntityById(1L)).thenReturn(user);
            accessibilityService.validateVisibility(content, 1L);
        }

        @Test
        void doesNotThrowWhenAuthor() {
            when(userService.getUserEntityById(1L)).thenReturn(user);
            accessibilityService.validateVisibility(content, 1L);
        }

        @Test
        void throwsWhenHiddenAndNotAuthorOrAdmin() {
            AccessibleContent hidden = new AccessibleContent() {
                @Override
                public Long getAuthorId() { return 99L; }
                @Override
                public Long getPostAuthorId() { return 99L; }
                @Override
                public Long getGroupOwnerId() { return null; }
                @Override
                public VisibilityStatus getVisibilityStatus() { return VisibilityStatus.HIDDEN; }
                @Override
                public ModerationStatus getModerationStatus() { return ModerationStatus.NONE; }
            };
            when(userService.getUserEntityById(1L)).thenReturn(user);
            assertThatThrownBy(() -> accessibilityService.validateVisibility(hidden, 1L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("cannot access");
        }
    }

    @Nested
    @DisplayName("validateModeration")
    class ValidateModeration {
        @Test
        void doesNotThrowWhenNone() {
            when(userService.getUserEntityById(1L)).thenReturn(user);
            accessibilityService.validateModeration(content, 1L);
        }

        @Test
        void throwsWhenPendingApprovalAndNotAuthorOrAdmin() {
            AccessibleContent pending = new AccessibleContent() {
                @Override
                public Long getAuthorId() { return 99L; }
                @Override
                public Long getPostAuthorId() { return 99L; }
                @Override
                public Long getGroupOwnerId() { return null; }
                @Override
                public VisibilityStatus getVisibilityStatus() { return VisibilityStatus.ACTIVE; }
                @Override
                public ModerationStatus getModerationStatus() { return ModerationStatus.PENDING_APPROVAL; }
            };
            when(userService.getUserEntityById(1L)).thenReturn(user);
            assertThatThrownBy(() -> accessibilityService.validateModeration(pending, 1L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("under moderation");
        }
    }
}
