package com.fackbook.Group.Service;

import com.fackbook.Group.DTO.GroupDTO;
import com.fackbook.Group.Entity.Group;
import com.fackbook.Group.Entity.GroupMember;
import com.fackbook.Group.Enum.ApprovalMode;
import com.fackbook.Group.Enum.JoinPolicy;
import com.fackbook.Group.Repository.GroupRepository;
import com.fackbook.TestFixtures;
import com.fackbook.User.Entity.User;
import com.fackbook.User.Enum.Status;
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
@DisplayName("GroupService tests")
class GroupServiceTest {

    @Mock
    private GroupRepository groupRepository;

    @Mock
    private GroupMemberService groupMemberService;

    @Mock
    private UserService userService;

    @Mock
    private com.fackbook.Shared.Helper.FileHelper fileHelper;

    @InjectMocks
    private GroupService groupService;

    private User owner;
    private Group group;
    private GroupDTO groupDTO;
    private Pageable pageable;

    @BeforeEach
    void setUp() {
        owner = TestFixtures.groupOwner(1L);
        group = TestFixtures.group(10L, owner);
        groupDTO = GroupDTO.builder()
                .name("New Group")
                .description("Desc")
                .joinPolicy(JoinPolicy.PUBLIC)
                .approvalMode(ApprovalMode.NONE)
                .status(Status.ACTIVE)
                .build();
        pageable = TestFixtures.pageable();
    }

    @Nested
    @DisplayName("getGroupEntityByGroupId")
    class GetGroupEntityByGroupId {
        @Test
        void returnsGroupWhenFound() {
            when(groupRepository.findById(10L)).thenReturn(Optional.of(group));
            Group result = groupService.getGroupEntityByGroupId(10L);
            assertThat(result).isSameAs(group);
        }

        @Test
        void throwsWhenNotFound() {
            when(groupRepository.findById(999L)).thenReturn(Optional.empty());
            assertThatThrownBy(() -> groupService.getGroupEntityByGroupId(999L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Group Not Found");
        }
    }

    @Nested
    @DisplayName("getGroupsByUserId")
    class GetGroupsByUserId {
        @Test
        void returnsPageOfGroups() {
            when(groupRepository.findByUser_Id(1L, pageable)).thenReturn(new PageImpl<>(List.of(group)));
            Page<GroupDTO> result = groupService.getGroupsByUserId(1L, pageable);
            assertThat(result).isNotEmpty();
        }
    }

    @Nested
    @DisplayName("getGroupsByStatus")
    class GetGroupsByStatus {
        @Test
        void returnsPageOfGroups() {
            when(groupRepository.findByStatus(Status.ACTIVE, pageable)).thenReturn(new PageImpl<>(List.of(group)));
            Page<GroupDTO> result = groupService.getGroupsByStatus(Status.ACTIVE, pageable);
            assertThat(result).isNotEmpty();
        }
    }

    @Nested
    @DisplayName("getGroupByName")
    class GetGroupByName {
        @Test
        void returnsDTOWhenFound() {
            when(groupRepository.findByNameIgnoreCase("Test Group")).thenReturn(Optional.of(group));
            GroupDTO result = groupService.getGroupByName("Test Group");
            assertThat(result).isNotNull();
            assertThat(result.getName()).isEqualTo("Test Group");
        }

        @Test
        void throwsWhenNotFound() {
            when(groupRepository.findByNameIgnoreCase("Missing")).thenReturn(Optional.empty());
            assertThatThrownBy(() -> groupService.getGroupByName("Missing"))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Group Not Found");
        }
    }

    @Nested
    @DisplayName("createNewGroup")
    class CreateNewGroup {
        @Test
        void createsGroupAndReturnsDTO() {
            when(userService.getUserEntityById(1L)).thenReturn(owner);
            when(groupRepository.findByNameIgnoreCase("New Group")).thenReturn(Optional.empty());
            when(groupRepository.save(any(Group.class))).thenAnswer(i -> {
                Group g = i.getArgument(0);
                g.setId(20L);
                return g;
            });

            GroupDTO result = groupService.createNewGroup(1L, groupDTO);

            assertThat(result).isNotNull();
            assertThat(result.getName()).isEqualTo("New Group");
            verify(groupRepository, org.mockito.Mockito.times(2)).save(any(Group.class));
        }

        @Test
        void defaultsStatusToActiveWhenNull() {
            groupDTO.setStatus(null);
            when(userService.getUserEntityById(1L)).thenReturn(owner);
            when(groupRepository.findByNameIgnoreCase("New Group")).thenReturn(Optional.empty());
            when(groupRepository.save(any(Group.class))).thenAnswer(i -> {
                Group g = i.getArgument(0);
                g.setId(20L);
                return g;
            });

            GroupDTO result = groupService.createNewGroup(1L, groupDTO);

            assertThat(result).isNotNull();
            verify(groupRepository, org.mockito.Mockito.times(2)).save(any(Group.class));
        }

        @Test
        void throwsWhenNameAlreadyUsed() {
            when(userService.getUserEntityById(1L)).thenReturn(owner);
            when(groupRepository.findByNameIgnoreCase("New Group")).thenReturn(Optional.of(group));

            assertThatThrownBy(() -> groupService.createNewGroup(1L, groupDTO))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Name Already Used");
        }
    }

    @Nested
    @DisplayName("updateGroup")
    class UpdateGroup {
        @Test
        void updatesAndReturnsDTO() {
            when(groupRepository.findById(10L)).thenReturn(Optional.of(group));
            when(groupRepository.existsByNameIgnoreCaseAndIdNot("Updated", 10L)).thenReturn(false);
            when(groupRepository.save(any(Group.class))).thenAnswer(i -> i.getArgument(0));
            groupDTO.setName("Updated");

            GroupDTO result = groupService.updateGroup(10L, groupDTO);

            assertThat(result).isNotNull();
            verify(groupRepository).save(group);
        }

        @Test
        void throwsWhenNameTakenByOtherGroup() {
            when(groupRepository.findById(10L)).thenReturn(Optional.of(group));
            when(groupRepository.existsByNameIgnoreCaseAndIdNot("Taken", 10L)).thenReturn(true);
            groupDTO.setName("Taken");

            assertThatThrownBy(() -> groupService.updateGroup(10L, groupDTO))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Name Already Used");
        }
    }

    @Nested
    @DisplayName("activeGroup / inactiveGroup / banGroup / deleteGroup")
    class ChangeGroupStatus {
        @Test
        void activeGroupUpdatesStatusAndSaves() {
            group.setStatus(Status.INACTIVE);
            when(groupRepository.findById(10L)).thenReturn(Optional.of(group));
            when(groupRepository.save(any(Group.class))).thenAnswer(i -> i.getArgument(0));

            GroupDTO result = groupService.activeGroup(10L);

            assertThat(result).isNotNull();
            assertThat(group.getStatus()).isEqualTo(Status.ACTIVE);
            verify(groupRepository).save(group);
        }

        @Test
        void deleteGroupSetsDeletedAndSaves() {
            when(groupRepository.findById(10L)).thenReturn(Optional.of(group));
            when(groupRepository.save(any(Group.class))).thenAnswer(i -> i.getArgument(0));

            GroupDTO result = groupService.deleteGroup(10L);

            assertThat(result).isNotNull();
            assertThat(group.getDeleted()).isTrue();
            verify(groupRepository).save(group);
        }
    }

    @Nested
    @DisplayName("getAllGroups")
    class GetAllGroups {
        @Test
        void returnsPageOfGroups() {
            when(groupRepository.findAll(pageable)).thenReturn(new PageImpl<>(List.of(group)));
            Page<GroupDTO> result = groupService.getAllGroups(pageable);
            assertThat(result).isNotEmpty();
        }
    }
}
