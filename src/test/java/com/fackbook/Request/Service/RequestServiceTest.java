package com.fackbook.Request.Service;

import com.fackbook.Group.Enum.JoinPolicy;
import com.fackbook.Group.Repository.GroupRepository;
import com.fackbook.Notification.NotificationService;
import com.fackbook.Post.Repository.PostRepository;
import com.fackbook.Request.DTO.RequestDTO;
import com.fackbook.Request.Entity.Request;
import com.fackbook.Request.Enum.RequestActionType;
import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.Request.Enum.Status;
import com.fackbook.Request.Repository.RequestRepository;
import com.fackbook.TestFixtures;
import com.fackbook.User.Entity.User;
import com.fackbook.User.Service.UserService;
import com.fackbook.Friend.Repository.FriendshipRepository;
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
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("RequestService tests")
class RequestServiceTest {

    @Mock
    private UserService userService;

    @Mock
    private GroupRepository groupRepository;

    @Mock
    private FriendshipRepository friendshipRepository;

    @Mock
    private PostRepository postRepository;

    @Mock
    private RequestRepository requestRepository;

    @Mock
    private NotificationService notificationService;

    @Mock
    private com.fackbook.Group.Service.GroupMemberService groupMemberService;

    @InjectMocks
    private RequestService requestService;

    private User user;
    private Request request;
    private RequestDTO requestDTO;
    private Pageable pageable;

    @BeforeEach
    void setUp() {
        user = TestFixtures.user(1L);
        request = TestFixtures.request(1L, user, 10L, RequestTargetType.GROUP,
                RequestActionType.GROUP_JOIN_REQUEST, Status.PENDING);
        requestDTO = RequestDTO.builder()
                .targetType(RequestTargetType.GROUP)
                .actionType(RequestActionType.GROUP_JOIN_REQUEST)
                .build();
        pageable = TestFixtures.pageable();
    }

    @Nested
    @DisplayName("createNewRequest")
    class CreateNewRequest {
        @Test
        void createsGroupJoinRequestWhenGroupExistsAndNotPublic() {
            var group = TestFixtures.group(10L, user);
            group.setJoinPolicy(JoinPolicy.REQUEST);
            when(userService.getUserEntityById(1L)).thenReturn(user);
            when(groupRepository.findById(10L)).thenReturn(Optional.of(group));
            when(requestRepository.save(any(Request.class))).thenAnswer(i -> {
                Request r = i.getArgument(0);
                r.setId(1L);
                return r;
            });

            Request result = requestService.createNewRequest(1L, 10L, requestDTO);

            assertThat(result).isNotNull();
            verify(requestRepository).save(any(Request.class));
            verify(notificationService).sendNotificationViaRequest(any(Request.class));
        }

        @Test
        void throwsWhenGroupIsPublic() {
            var group = TestFixtures.group(10L, user);
            group.setJoinPolicy(JoinPolicy.PUBLIC);
            when(userService.getUserEntityById(1L)).thenReturn(user);
            when(groupRepository.findById(10L)).thenReturn(Optional.of(group));

            assertThatThrownBy(() -> requestService.createNewRequest(1L, 10L, requestDTO))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("does not require");
        }
    }

    @Nested
    @DisplayName("getRequestEntityById / getRequestById")
    class GetRequest {
        @Test
        void getRequestEntityByIdReturnsRequestWhenFound() {
            when(requestRepository.findById(1L)).thenReturn(Optional.of(request));
            Request result = requestService.getRequestEntityById(1L);
            assertThat(result).isSameAs(request);
        }

        @Test
        void getRequestEntityByIdThrowsWhenNotFound() {
            when(requestRepository.findById(999L)).thenReturn(Optional.empty());
            assertThatThrownBy(() -> requestService.getRequestEntityById(999L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Request Not Found");
        }

        @Test
        void getRequestByIdReturnsDTOWhenFound() {
            when(requestRepository.findById(1L)).thenReturn(Optional.of(request));
            var result = requestService.getRequestById(1L);
            assertThat(result).isNotNull();
        }
    }

    @Nested
    @DisplayName("getRequestsByUserId")
    class GetRequestsByUserId {
        @Test
        void returnsPageOfRequests() {
            when(requestRepository.findByUser_Id(1L, pageable)).thenReturn(new PageImpl<>(List.of(request)));
            Page<?> result = requestService.getRequestsByUserId(1L, pageable);
            assertThat(result).isNotEmpty();
        }
    }

    @Nested
    @DisplayName("getRequestByUserIdAndTargetIdAndActionType")
    class GetRequestByUserIdAndTargetIdAndActionType {
        @Test
        void returnsDTOWhenFound() {
            when(requestRepository.findByUser_IdAndTargetIdAndActionType(1L, 10L, RequestActionType.GROUP_JOIN_REQUEST))
                    .thenReturn(Optional.of(request));
            var result = requestService.getRequestByUserIdAndTargetIdAndActionType(1L, 10L, RequestActionType.GROUP_JOIN_REQUEST);
            assertThat(result).isNotNull();
        }

        @Test
        void throwsWhenNotFound() {
            when(requestRepository.findByUser_IdAndTargetIdAndActionType(1L, 10L, RequestActionType.GROUP_JOIN_REQUEST))
                    .thenReturn(Optional.empty());
            assertThatThrownBy(() -> requestService.getRequestByUserIdAndTargetIdAndActionType(1L, 10L, RequestActionType.GROUP_JOIN_REQUEST))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Request Not Found");
        }
    }

    @Nested
    @DisplayName("getRequestByTargetIdAndActionType")
    class GetRequestByTargetIdAndActionType {
        @Test
        void returnsPageOfRequests() {
            when(requestRepository.findByTargetIdAndActionType(10L, RequestActionType.GROUP_JOIN_REQUEST, pageable))
                    .thenReturn(new PageImpl<>(List.of(request)));
            Page<?> result = requestService.getRequestByTargetIdAndActionType(10L, RequestActionType.GROUP_JOIN_REQUEST, pageable);
            assertThat(result).isNotEmpty();
        }
    }

    @Nested
    @DisplayName("setRequestAsAccepted / setRequestAsRejected")
    class ChangeRequestStatus {
        @Test
        void setRequestAsAcceptedUpdatesStatusAndSaves() {
            when(requestRepository.findById(1L)).thenReturn(Optional.of(request));
            when(requestRepository.save(any(Request.class))).thenAnswer(i -> i.getArgument(0));
            when(groupMemberService.handleGroupMembership(any(Long.class), any(Long.class), any(Request.class)))
                    .thenReturn(null);

            var result = requestService.setRequestAsAccepted(1L);

            assertThat(result).isNotNull();
            assertThat(request.getStatus()).isEqualTo(Status.ACCEPTED);
            verify(requestRepository).save(request);
        }

        @Test
        void setRequestAsAcceptedThrowsWhenNotPending() {
            request.setStatus(Status.ACCEPTED);
            when(requestRepository.findById(1L)).thenReturn(Optional.of(request));

            assertThatThrownBy(() -> requestService.setRequestAsAccepted(1L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Already");
        }

        @Test
        void setRequestAsRejectedUpdatesStatusAndSaves() {
            when(requestRepository.findById(1L)).thenReturn(Optional.of(request));
            when(requestRepository.save(any(Request.class))).thenAnswer(i -> i.getArgument(0));
            when(groupMemberService.handleGroupMembership(any(Long.class), any(Long.class), any(Request.class)))
                    .thenReturn(null);

            var result = requestService.setRequestAsRejected(1L);

            assertThat(result).isNotNull();
            verify(requestRepository).save(request);
        }
    }

    @Nested
    @DisplayName("saveRequest")
    class SaveRequest {
        @Test
        void savesRequest() {
            requestService.saveRequest(request);
            verify(requestRepository).save(request);
        }
    }

    @Nested
    @DisplayName("getGroupJoinRequests / getGroupInviteRequests / getPendingRequests")
    class GetRequestsByType {
        @Test
        void getGroupJoinRequestsReturnsPage() {
            when(requestRepository.findByActionType(RequestActionType.GROUP_JOIN_REQUEST, pageable))
                    .thenReturn(new PageImpl<>(List.of(request)));
            Page<?> result = requestService.getGroupJoinRequests(pageable);
            assertThat(result).isNotEmpty();
        }

        @Test
        void getPendingRequestsReturnsPage() {
            when(requestRepository.findByStatus(Status.PENDING, pageable))
                    .thenReturn(new PageImpl<>(List.of(request)));
            Page<?> result = requestService.getPendingRequests(pageable);
            assertThat(result).isNotEmpty();
        }
    }
}
