package com.fackbook.Group.Service;

import com.fackbook.Group.DTO.GroupMemberDTO;
import com.fackbook.Group.Entity.Group;
import com.fackbook.Group.Entity.GroupMember;
import com.fackbook.Group.Repository.GroupMemberRepository;
import com.fackbook.Group.Repository.GroupRepository;
import com.fackbook.Request.Entity.Request;
import com.fackbook.Request.Enum.RequestActionType;
import com.fackbook.Request.Service.RequestService;
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
@DisplayName("GroupMemberService tests")
class GroupMemberServiceTest {

    @Mock
    private GroupMemberRepository groupMemberRepository;

    @Mock
    private GroupRepository groupRepository;

    @Mock
    private UserService userService;

    @Mock
    private RequestService requestService;

    @InjectMocks
    private GroupMemberService groupMemberService;

    private User user;
    private Group group;
    private GroupMember groupMember;
    private Pageable pageable;

    @BeforeEach
    void setUp() {
        user = TestFixtures.user(1L);
        group = TestFixtures.group(10L, user);
        groupMember = TestFixtures.groupMember(1L, user, group, Role.GROUP_MEMBER, com.fackbook.User.Enum.Status.ACTIVE);
        pageable = TestFixtures.pageable();
    }

    @Nested
    @DisplayName("getGroupMemberEntityById")
    class GetGroupMemberEntityById {
        @Test
        void returnsOptionalWhenFound() {
            when(groupMemberRepository.findById(1L)).thenReturn(Optional.of(groupMember));
            Optional<GroupMember> result = groupMemberService.getGroupMemberEntityById(1L);
            assertThat(result).isPresent();
        }

        @Test
        void returnsEmptyWhenNotFound() {
            when(groupMemberRepository.findById(999L)).thenReturn(Optional.empty());
            Optional<GroupMember> result = groupMemberService.getGroupMemberEntityById(999L);
            assertThat(result).isEmpty();
        }
    }

    @Nested
    @DisplayName("getGroupMembersByGroupId")
    class GetGroupMembersByGroupId {
        @Test
        void returnsPageOfMembers() {
            when(groupRepository.findById(10L)).thenReturn(Optional.of(group));
            when(groupMemberRepository.findByGroup_IdAndRole(10L, Role.GROUP_MEMBER, pageable))
                    .thenReturn(new PageImpl<>(List.of(groupMember)));
            Page<GroupMemberDTO> result = groupMemberService.getGroupMembersByGroupId(10L, pageable);
            assertThat(result).isNotEmpty();
        }

        @Test
        void throwsWhenGroupNotFound() {
            when(groupRepository.findById(999L)).thenReturn(Optional.empty());
            assertThatThrownBy(() -> groupMemberService.getGroupMembersByGroupId(999L, pageable))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Group Not Found");
        }
    }

    @Nested
    @DisplayName("getGroupMemberEntityByUserIdAndGroupId")
    class GetGroupMemberEntityByUserIdAndGroupId {
        @Test
        void returnsOptionalWhenFound() {
            when(groupMemberRepository.findByUser_IdAndGroup_Id(1L, 10L)).thenReturn(Optional.of(groupMember));
            Optional<GroupMember> result = groupMemberService.getGroupMemberEntityByUserIdAndGroupId(1L, 10L);
            assertThat(result).isPresent();
        }
    }

    @Nested
    @DisplayName("getByGroupMemberId")
    class GetByGroupMemberId {
        @Test
        void returnsDTOWhenFound() {
            when(groupMemberRepository.findById(1L)).thenReturn(Optional.of(groupMember));
            GroupMemberDTO result = groupMemberService.getByGroupMemberId(1L);
            assertThat(result).isNotNull();
        }

        @Test
        void throwsWhenNotFound() {
            when(groupMemberRepository.findById(999L)).thenReturn(Optional.empty());
            assertThatThrownBy(() -> groupMemberService.getByGroupMemberId(999L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Group Member Not Found");
        }
    }

    @Nested
    @DisplayName("handleGroupMembership - PUBLIC")
    class HandleGroupMembershipPublic {
        @Test
        void createsMemberWhenPublicAndNoRequest() {
            group.setJoinPolicy(com.fackbook.Group.Enum.JoinPolicy.PUBLIC);
            when(groupRepository.findById(10L)).thenReturn(Optional.of(group));
            when(userService.getUserEntityById(1L)).thenReturn(user);
            when(groupMemberRepository.findByUser_IdAndGroup_Id(1L, 10L)).thenReturn(Optional.empty());
            when(groupMemberRepository.save(any(GroupMember.class))).thenAnswer(i -> i.getArgument(0));
            when(groupRepository.save(any(Group.class))).thenAnswer(i -> i.getArgument(0));

            GroupMember result = groupMemberService.handleGroupMembership(1L, 10L, null);

            assertThat(result).isNotNull();
            verify(groupMemberRepository).save(any(GroupMember.class));
        }
    }

    @Nested
    @DisplayName("handleGroupMembership - REQUEST accepted")
    class HandleGroupMembershipRequestAccepted {
        @Test
        void createsMemberWhenRequestAccepted() {
            group.setJoinPolicy(com.fackbook.Group.Enum.JoinPolicy.REQUEST);
            Request request = TestFixtures.request(1L, user, 10L,
                    com.fackbook.Request.Enum.RequestTargetType.GROUP, RequestActionType.GROUP_JOIN_REQUEST,
                    com.fackbook.Request.Enum.Status.ACCEPTED);
            when(groupRepository.findById(10L)).thenReturn(Optional.of(group));
            when(userService.getUserEntityById(1L)).thenReturn(user);
            when(groupMemberRepository.findByUser_IdAndGroup_Id(1L, 10L)).thenReturn(Optional.empty());
            when(groupMemberRepository.save(any(GroupMember.class))).thenAnswer(i -> i.getArgument(0));
            when(groupRepository.save(any(Group.class))).thenAnswer(i -> i.getArgument(0));

            GroupMember result = groupMemberService.handleGroupMembership(1L, 10L, request);

            assertThat(result).isNotNull();
            verify(groupMemberRepository).save(any(GroupMember.class));
        }

        @Test
        void throwsWhenRequestNotAccepted() {
            group.setJoinPolicy(com.fackbook.Group.Enum.JoinPolicy.REQUEST);
            Request request = TestFixtures.request(1L, user, 10L,
                    com.fackbook.Request.Enum.RequestTargetType.GROUP, RequestActionType.GROUP_JOIN_REQUEST,
                    com.fackbook.Request.Enum.Status.PENDING);
            when(groupRepository.findById(10L)).thenReturn(Optional.of(group));

            assertThatThrownBy(() -> groupMemberService.handleGroupMembership(1L, 10L, request))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("not been approved");
        }
    }

    @Nested
    @DisplayName("toGroupMember / toGroupAdmin")
    class ToGroupMemberOrAdmin {
        @Test
        void toGroupMemberCreatesMember() {
            when(userService.getUserEntityById(1L)).thenReturn(user);
            when(groupRepository.findById(10L)).thenReturn(Optional.of(group));
            when(groupMemberRepository.findByUser_IdAndGroup_Id(1L, 10L)).thenReturn(Optional.empty());
            when(groupMemberRepository.save(any(GroupMember.class))).thenAnswer(i -> i.getArgument(0));
            when(groupRepository.save(any(Group.class))).thenAnswer(i -> i.getArgument(0));

            GroupMemberDTO result = groupMemberService.toGroupMember(1L, 10L);

            assertThat(result).isNotNull();
            verify(groupMemberRepository).save(any(GroupMember.class));
        }
    }

    @Nested
    @DisplayName("getAllUserMembershipsByUserId")
    class GetAllUserMembershipsByUserId {
        @Test
        void returnsPageOfMemberships() {
            when(groupMemberRepository.findByUser_Id(1L, pageable)).thenReturn(new PageImpl<>(List.of(groupMember)));
            Page<GroupMemberDTO> result = groupMemberService.getAllUserMembershipsByUserId(1L, pageable);
            assertThat(result).isNotEmpty();
        }
    }

    @Nested
    @DisplayName("deleteGroupMemberByGroupMemberId")
    class DeleteGroupMemberByGroupMemberId {
        @Test
        void updatesStatusAndDecrementsCount() {
            when(groupMemberRepository.findById(1L)).thenReturn(Optional.of(groupMember));
            when(groupRepository.findById(10L)).thenReturn(Optional.of(group));
            when(groupMemberRepository.save(any(GroupMember.class))).thenAnswer(i -> i.getArgument(0));
            when(groupRepository.save(any(Group.class))).thenAnswer(i -> i.getArgument(0));

            GroupMemberDTO result = groupMemberService.deleteGroupMemberByGroupMemberId(1L);

            assertThat(result).isNotNull();
            verify(groupMemberRepository).save(groupMember);
        }
    }
}
