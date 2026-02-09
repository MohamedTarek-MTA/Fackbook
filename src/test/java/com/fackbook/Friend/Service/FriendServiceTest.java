package com.fackbook.Friend.Service;

import com.fackbook.Friend.Entity.Friendship;
import com.fackbook.Friend.Enum.Status;
import com.fackbook.Friend.Repository.FriendshipRepository;
import com.fackbook.Request.Entity.Request;
import com.fackbook.Request.Enum.RequestActionType;
import com.fackbook.Request.Service.RequestService;
import com.fackbook.TestFixtures;
import com.fackbook.User.Entity.User;
import com.fackbook.User.Service.UserService;
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
@DisplayName("FriendService tests")
class FriendServiceTest {

    @Mock
    private FriendshipRepository friendshipRepository;

    @Mock
    private UserService userService;

    @Mock
    private RequestService requestService;

    @InjectMocks
    private FriendService friendService;

    private User user1;
    private User user2;
    private Friendship friendship;
    private Request friendshipRequest;
    private Pageable pageable;

    @BeforeEach
    void setUp() {
        user1 = TestFixtures.user(1L);
        user2 = TestFixtures.user(2L);
        user2.setEmail("user2@test.com");
        friendship = TestFixtures.friendship(1L, user1, user2, Status.FRIENDS);
        friendshipRequest = TestFixtures.request(1L, user1, 2L,
                com.fackbook.Request.Enum.RequestTargetType.USER, RequestActionType.FRIENDSHIP_REQUEST, com.fackbook.Request.Enum.Status.PENDING);
        pageable = TestFixtures.pageable();
    }

    @Nested
    @DisplayName("getUserFriendshipsByUserId")
    class GetUserFriendshipsByUserId {
        @Test
        void returnsPageOfFriendships() {
            when(friendshipRepository.findByUser_IdAndStatus(1L, Status.FRIENDS, pageable))
                    .thenReturn(new PageImpl<>(List.of(friendship)));
            Page<?> result = friendService.getUserFriendshipsByUserId(1L, pageable);
            assertThat(result).isNotEmpty();
        }
    }

    @Nested
    @DisplayName("getFriendFriendshipsByFriendId")
    class GetFriendFriendshipsByFriendId {
        @Test
        void returnsPageOfFriendships() {
            when(friendshipRepository.findByFriend_idAndStatus(2L, Status.FRIENDS, pageable))
                    .thenReturn(new PageImpl<>(List.of(friendship)));
            Page<?> result = friendService.getFriendFriendshipsByFriendId(2L, pageable);
            assertThat(result).isNotEmpty();
        }
    }

    @Nested
    @DisplayName("getFriendshipEntityByUserIdAndFriendId")
    class GetFriendshipEntityByUserIdAndFriendId {
        @Test
        void returnsOptionalWhenFound() {
            when(friendshipRepository.findByUser_IdAndFriend_Id(1L, 2L)).thenReturn(Optional.of(friendship));
            Optional<Friendship> result = friendService.getFriendshipEntityByUserIdAndFriendId(1L, 2L);
            assertThat(result).isPresent();
        }

        @Test
        void normalizesOrderSoUserMinFriendMax() {
            when(friendshipRepository.findByUser_IdAndFriend_Id(1L, 2L)).thenReturn(Optional.of(friendship));
            Optional<Friendship> result = friendService.getFriendshipEntityByUserIdAndFriendId(2L, 1L);
            assertThat(result).isPresent();
        }

        @Test
        void throwsWhenSameUser() {
            assertThatThrownBy(() -> friendService.getFriendshipEntityByUserIdAndFriendId(1L, 1L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("himself");
        }
    }

    @Nested
    @DisplayName("getFriendshipByUserIdAndFriendId")
    class GetFriendshipByUserIdAndFriendId {
        @Test
        void returnsDTOWhenFriends() {
            when(friendshipRepository.findByUser_IdAndFriend_Id(1L, 2L)).thenReturn(Optional.of(friendship));
            var result = friendService.getFriendshipByUserIdAndFriendId(1L, 2L);
            assertThat(result).isNotNull();
        }

        @Test
        void throwsWhenNotFound() {
            when(friendshipRepository.findByUser_IdAndFriend_Id(1L, 2L)).thenReturn(Optional.empty());
            assertThatThrownBy(() -> friendService.getFriendshipByUserIdAndFriendId(1L, 2L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Friendship Not Found");
        }

        @Test
        void throwsWhenStatusNotFriends() {
            friendship.setStatus(Status.NOT_FRIENDS);
            when(friendshipRepository.findByUser_IdAndFriend_Id(1L, 2L)).thenReturn(Optional.of(friendship));
            assertThatThrownBy(() -> friendService.getFriendshipByUserIdAndFriendId(1L, 2L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("ended");
        }
    }

    @Nested
    @DisplayName("getFriendshipByFriendshipId")
    class GetFriendshipByFriendshipId {
        @Test
        void returnsDTOWhenFound() {
            when(friendshipRepository.findById(1L)).thenReturn(Optional.of(friendship));
            var result = friendService.getFriendshipByFriendshipId(1L);
            assertThat(result).isNotNull();
        }

        @Test
        void throwsWhenNotFound() {
            when(friendshipRepository.findById(999L)).thenReturn(Optional.empty());
            assertThatThrownBy(() -> friendService.getFriendshipByFriendshipId(999L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Friendship Not Found");
        }
    }

    @Nested
    @DisplayName("getAllFriendships / getAllEndedFriendships")
    class GetAllFriendships {
        @Test
        void getAllFriendshipsReturnsPage() {
            when(friendshipRepository.findByStatus(Status.FRIENDS, pageable))
                    .thenReturn(new PageImpl<>(List.of(friendship)));
            Page<?> result = friendService.getAllFriendships(pageable);
            assertThat(result).isNotEmpty();
        }

        @Test
        void getAllEndedFriendshipsReturnsPage() {
            when(friendshipRepository.findByStatus(Status.NOT_FRIENDS, pageable))
                    .thenReturn(new PageImpl<>(List.of()));
            Page<?> result = friendService.getAllEndedFriendships(pageable);
            assertThat(result).isEmpty();
        }
    }

    @Nested
    @DisplayName("approveFriendshipRequest")
    class ApproveFriendshipRequest {
        @Test
        void createsNewFriendshipWhenNoneExists() {
            when(requestService.getRequestEntityById(1L)).thenReturn(friendshipRequest);
            when(friendshipRepository.findByUser_IdAndFriend_Id(1L, 2L)).thenReturn(Optional.empty());
            when(userService.getUserEntityById(1L)).thenReturn(user1);
            when(userService.getUserEntityById(2L)).thenReturn(user2);
            when(friendshipRepository.save(any(Friendship.class))).thenAnswer(i -> i.getArgument(0));

            var result = friendService.approveFriendshipRequest(1L);

            assertThat(result).isNotNull();
            verify(requestService).saveRequest(friendshipRequest);
            verify(friendshipRepository).save(any(Friendship.class));
        }

        @Test
        void throwsWhenRequestNotPending() {
            friendshipRequest.setStatus(com.fackbook.Request.Enum.Status.ACCEPTED);
            when(requestService.getRequestEntityById(1L)).thenReturn(friendshipRequest);

            assertThatThrownBy(() -> friendService.approveFriendshipRequest(1L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("already");
        }
    }

    @Nested
    @DisplayName("endFriendshipByUserIdAndFriendId")
    class EndFriendshipByUserIdAndFriendId {
        @Test
        void setsStatusToNotFriendsAndSaves() {
            when(friendshipRepository.findByUser_IdAndFriend_Id(1L, 2L)).thenReturn(Optional.of(friendship));
            when(friendshipRepository.save(any(Friendship.class))).thenAnswer(i -> i.getArgument(0));

            var result = friendService.endFriendshipByUserIdAndFriendId(1L, 2L);

            assertThat(result).isNotNull();
            assertThat(friendship.getStatus()).isEqualTo(Status.NOT_FRIENDS);
            verify(friendshipRepository).save(friendship);
        }

        @Test
        void throwsWhenFriendshipNotFound() {
            when(friendshipRepository.findByUser_IdAndFriend_Id(1L, 2L)).thenReturn(Optional.empty());

            assertThatThrownBy(() -> friendService.endFriendshipByUserIdAndFriendId(1L, 2L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Doesn't Exists");
        }
    }
}
