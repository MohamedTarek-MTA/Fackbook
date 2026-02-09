package com.fackbook.React.Service;

import com.fackbook.Comment.Repository.CommentRepository;
import com.fackbook.Notification.NotificationService;
import com.fackbook.Post.Entity.Post;
import com.fackbook.Post.Enum.VisibilityStatus;
import com.fackbook.Post.Repository.PostRepository;
import com.fackbook.React.Entity.React;
import com.fackbook.React.Enum.ReactType;
import com.fackbook.React.Repository.ReactRepository;
import com.fackbook.Reply.Repository.ReplyRepository;
import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.TestFixtures;
import com.fackbook.User.Entity.User;
import com.fackbook.User.Repository.UserRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("ReactService tests")
class ReactServiceTest {

    @Mock
    private UserRepository userRepository;

    @Mock
    private PostRepository postRepository;

    @Mock
    private CommentRepository commentRepository;

    @Mock
    private ReplyRepository replyRepository;

    @Mock
    private ReactRepository reactRepository;

    @Mock
    private NotificationService notificationService;

    @InjectMocks
    private ReactService reactService;

    private User user;
    private Post post;

    @BeforeEach
    void setUp() {
        user = TestFixtures.user(1L);
        post = TestFixtures.post(10L, user);
    }

    @Nested
    @DisplayName("react")
    class React {
        @Test
        void createsNewReactWhenNoneExists() {
            when(userRepository.findById(1L)).thenReturn(Optional.of(user));
            when(postRepository.findById(10L)).thenReturn(Optional.of(post));
            when(reactRepository.findByUser_IdAndTargetIdAndTargetType(1L, 10L, RequestTargetType.POST))
                    .thenReturn(Optional.empty());
            when(reactRepository.save(any(com.fackbook.React.Entity.React.class))).thenAnswer(i -> {
                com.fackbook.React.Entity.React r = i.getArgument(0);
                r.setId(1L);
                return r;
            });
            when(postRepository.save(any(Post.class))).thenAnswer(i -> i.getArgument(0));

            reactService.react(1L, 10L, RequestTargetType.POST, ReactType.LIKE);

            verify(reactRepository).save(any(com.fackbook.React.Entity.React.class));
            verify(notificationService).sendNotificationViaReact(any(com.fackbook.React.Entity.React.class));
        }

        @Test
        void removesReactWhenSameTypeClickedAgain() {
            com.fackbook.React.Entity.React existing = com.fackbook.React.Entity.React.builder()
                    .id(1L)
                    .user(user)
                    .targetId(10L)
                    .targetType(RequestTargetType.POST)
                    .reactType(ReactType.LIKE)
                    .build();
            when(postRepository.findById(10L)).thenReturn(Optional.of(post));
            when(reactRepository.findByUser_IdAndTargetIdAndTargetType(1L, 10L, RequestTargetType.POST))
                    .thenReturn(Optional.of(existing));
            when(postRepository.save(any(Post.class))).thenAnswer(i -> i.getArgument(0));

            reactService.react(1L, 10L, RequestTargetType.POST, ReactType.LIKE);

            verify(reactRepository).delete(existing);
        }

        @Test
        void throwsWhenPostNotFound() {
            when(postRepository.findById(999L)).thenReturn(Optional.empty());

            assertThatThrownBy(() -> reactService.react(1L, 999L, RequestTargetType.POST, ReactType.LIKE))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Post Not Found");
        }
    }

    @Nested
    @DisplayName("getReactionSummary")
    class GetReactionSummary {
        @Test
        void returnsSummaryMap() {
            when(postRepository.findById(10L)).thenReturn(Optional.of(post));
            when(reactRepository.countByTargetGrouped(RequestTargetType.POST, 10L))
                    .thenReturn(java.util.List.of(new Object[]{ReactType.LIKE, 5L}, new Object[]{ReactType.LOVE, 2L}));

            var result = reactService.getReactionSummary(10L, RequestTargetType.POST);

            assertThat(result).containsEntry(ReactType.LIKE, 5L).containsEntry(ReactType.LOVE, 2L);
        }
    }
}
