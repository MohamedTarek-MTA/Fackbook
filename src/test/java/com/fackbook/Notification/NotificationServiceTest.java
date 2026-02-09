package com.fackbook.Notification;

import com.fackbook.React.Entity.React;
import com.fackbook.React.Enum.ReactType;
import com.fackbook.Request.Entity.Request;
import com.fackbook.Request.Enum.RequestActionType;
import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.TestFixtures;
import com.fackbook.User.Entity.User;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("NotificationService tests")
class NotificationServiceTest {

    @Mock
    private com.fackbook.Group.Repository.GroupRepository groupRepository;

    @Mock
    private com.fackbook.Post.Repository.PostRepository postRepository;

    @Mock
    private com.fackbook.Comment.Repository.CommentRepository commentRepository;

    @Mock
    private com.fackbook.Reply.Repository.ReplyRepository replyRepository;

    @Mock
    private NotificationProducer notificationProducer;

    @InjectMocks
    private NotificationService notificationService;

    private User user;
    private Request request;

    @BeforeEach
    void setUp() {
        user = TestFixtures.user(1L);
        request = TestFixtures.request(1L, user, 2L, RequestTargetType.USER,
                RequestActionType.FRIENDSHIP_REQUEST, com.fackbook.Request.Enum.Status.PENDING);
    }

    @Nested
    @DisplayName("sendNotificationViaRequest")
    class SendNotificationViaRequest {
        @Test
        void sendsNotificationForFriendshipRequest() {
            notificationService.sendNotificationViaRequest(request);

            ArgumentCaptor<Notification> captor = ArgumentCaptor.forClass(Notification.class);
            verify(notificationProducer).sendNotification(captor.capture());
            Notification n = captor.getValue();
            assertThat(n.getFromUserId()).isEqualTo("1");
            assertThat(n.getMessage()).contains("friend request");
        }
    }

    @Nested
    @DisplayName("sendNotificationViaReact")
    class SendNotificationViaReact {
        @Test
        void sendsNotificationForPostReact() {
            React react = React.builder()
                    .id(1L)
                    .user(user)
                    .targetId(10L)
                    .targetType(RequestTargetType.POST)
                    .reactType(ReactType.LIKE)
                    .build();
            when(postRepository.findById(10L)).thenReturn(Optional.of(TestFixtures.post(10L, user)));

            notificationService.sendNotificationViaReact(react);

            ArgumentCaptor<Notification> captor = ArgumentCaptor.forClass(Notification.class);
            verify(notificationProducer).sendNotification(captor.capture());
            assertThat(captor.getValue().getMessage()).contains("Reacted");
        }
    }
}
