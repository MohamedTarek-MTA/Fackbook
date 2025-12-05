package com.fackbook.Group.Service;

import com.fackbook.Group.DTO.GroupMemberDTO;
import com.fackbook.Group.Entity.GroupMember;
import com.fackbook.Group.Mapper.GroupMemberMapper;
import com.fackbook.Group.Repository.GroupMemberRepository;
import com.fackbook.Group.Repository.GroupRepository;
import com.fackbook.Request.Entity.Request;
import com.fackbook.Request.Enum.RequestActionType;
import com.fackbook.Request.Service.RequestService;
import com.fackbook.User.Enum.Role;
import com.fackbook.User.Enum.Status;
import com.fackbook.User.Service.UserService;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class GroupMemberService {
    private final GroupMemberRepository groupMemberRepository;
    private final GroupRepository groupRepository;
    private final UserService userService;
    private final RequestService requestService;

    public Optional<GroupMember> getGroupMemberEntityById(Long id){
        return groupMemberRepository.findById(id);
    }
    public Page<GroupMemberDTO> getGroupMembersByGroupId(Long groupId, Pageable pageable){
        var group = groupRepository.findById(groupId).orElseThrow(()->new IllegalArgumentException("Group Not Found !"));
        return groupMemberRepository.findByGroup_IdAndRole(group.getId(),Role.GROUP_MEMBER,pageable)
                .map(GroupMemberMapper::toDTO);
    }
    public Optional<GroupMember> getGroupMemberEntityByUserIdAndGroupId(Long userId,Long groupId){
        return groupMemberRepository.findByUser_IdAndGroup_Id(userId,groupId);
    }
    public Page<GroupMemberDTO> getGroupAdminsByGroupId(Long groupId, Pageable pageable){
        var group = groupRepository.findById(groupId).orElseThrow(()->new IllegalArgumentException("Group Not Found !"));
        return groupMemberRepository.findByGroup_IdAndRole(group.getId(),Role.GROUP_ADMIN,pageable)
                .map(GroupMemberMapper::toDTO);
    }
    public Page<GroupMemberDTO> getGroupMembersByGroupName(String groupName,Pageable pageable){
        var group = groupRepository.findByNameIgnoreCase(groupName).orElseThrow(()->
                new IllegalArgumentException("Group Not Found !"));
        return groupMemberRepository.findByGroup_NameIgnoreCase(group.getName(),pageable).map(GroupMemberMapper::toDTO);
    }
    @Transactional
    private GroupMember createNewGroupMember(Long userId, Long groupId, Role role){
        var groupMember = getGroupMemberEntityByUserIdAndGroupId(userId,groupId);
        var group = groupRepository.findById(groupId).orElseThrow(()->new IllegalArgumentException("Group Not Found !"));
        if(!group.getStatus().equals(Status.ACTIVE)){
            throw new IllegalArgumentException("Sorry Group Currently Not Active !!");
        }
        if(groupMember.isPresent()){
            GroupMember existsGroupMember = groupMember.get();
            if(existsGroupMember.getStatus().equals(Status.ACTIVE)){
                throw new IllegalArgumentException("Group Member Already Exists !!");
            }
            existsGroupMember.setStatus(Status.ACTIVE);
            existsGroupMember.setUpdatedAt(LocalDateTime.now());
            existsGroupMember.setRole(role);
            existsGroupMember.setDeleted(false);
            existsGroupMember.setDeletedAt(null);
            return groupMemberRepository.save(existsGroupMember);
        }
        GroupMember newGroupMember = GroupMember.builder()
                .user(userService.getUserEntityById(userId))
                .group(group)
                .role(role)
                .createdAt(LocalDateTime.now())
                .status(Status.ACTIVE)
                .deleted(false)
                .deletedAt(null)
                .build();
        group.getMembers().add(newGroupMember);
        group.setNumberOfMembers(group.getNumberOfMembers().add(BigInteger.ONE));
        groupRepository.save(group);
        return groupMemberRepository.save(newGroupMember);
    }
    @Transactional
    public GroupMember handleGroupMembership(Long userId, Long groupId, Request request) {

        var group = groupRepository.findById(groupId)
                .orElseThrow(() -> new IllegalArgumentException("Group not found!"));

        // Check group status
        if (!group.getStatus().equals(Status.ACTIVE)) {
            throw new IllegalArgumentException("Sorry, the group is currently not active!");
        }

        switch (group.getJoinPolicy()) {

            case PUBLIC:
                // No request needed, create membership immediately
                return createNewGroupMember(userId, groupId, Role.GROUP_MEMBER);

            case REQUEST:
                if (request == null || request.getActionType() != RequestActionType.GROUP_JOIN_REQUEST) {
                    throw new IllegalArgumentException("No valid join request found for this group.");
                }
                if (request.getStatus() != com.fackbook.Request.Enum.Status.ACCEPTED) {
                    throw new IllegalArgumentException("The membership request has not been approved yet.");
                }
                return createNewGroupMember(userId, groupId, Role.GROUP_MEMBER);

            case INVENT_ONLY:
                if (request == null || request.getActionType() != RequestActionType.GROUP_INVITE) {
                    throw new IllegalArgumentException("No valid invitation found for this user.");
                }
                if (request.getStatus() != com.fackbook.Request.Enum.Status.ACCEPTED) {
                    throw new IllegalArgumentException("The invitation has not been accepted yet.");
                }
                return createNewGroupMember(userId, groupId, Role.GROUP_MEMBER);

            default:
                throw new IllegalArgumentException("Unhandled join policy: " + group.getJoinPolicy());
        }
    }

    @CachePut(value = "groupMembersByUserAndGroupIDs",key = "#userId + '-' + #groupId")
    public GroupMemberDTO toGroupAdmin(Long userId,Long groupId){
        return GroupMemberMapper.toDTO(createNewGroupMember(userId,groupId,Role.GROUP_ADMIN));
    }
    @CachePut(value = "groupMembersByUserAndGroupIDs",key = "#userId + '-' + #groupId")
    public GroupMemberDTO toGroupMember(Long userId,Long groupId){
        return GroupMemberMapper.toDTO(createNewGroupMember(userId,groupId,Role.GROUP_MEMBER));
    }
    public Page<GroupMemberDTO> getAllGroupMembersByGroupNameAndUserName(String groupName,String userName,Pageable pageable){
        return groupMemberRepository.findByGroup_NameIgnoreCaseAndUser_NameContainingIgnoreCase(
                groupName,userName,pageable
        ).map(GroupMemberMapper::toDTO);
    }
    public Page<GroupMemberDTO> getAllGroupAdmins(Pageable pageable){
        return groupMemberRepository.findByRole(Role.GROUP_ADMIN,pageable).map(GroupMemberMapper::toDTO);
    }
    public Page<GroupMemberDTO> getAllGroupMembers(Pageable pageable){
        return groupMemberRepository.findByRole(Role.GROUP_MEMBER,pageable).map(GroupMemberMapper::toDTO);
    }
    @Cacheable(value = "groupMembersById",key = "#groupMemberId")
    public GroupMemberDTO getByGroupMemberId(Long groupMemberId){
        return GroupMemberMapper.toDTO(
                getGroupMemberEntityById(groupMemberId).orElseThrow(()->
                        new IllegalArgumentException("Group Member Not Found !!"))
        );
    }
    @Transactional
    private GroupMemberDTO changeGroupMemberStatusByGroupMemberId(Long groupMemberId,Status status,Boolean deleted){
        var groupMember = getGroupMemberEntityById(groupMemberId);
        if(groupMember.isEmpty()){
            throw new IllegalArgumentException("Group Member Not Exists !!");
        }
        GroupMember exitsGroupMember = groupMember.get();
       Optional.ofNullable(status).ifPresent(exitsGroupMember::setStatus);
       Optional.ofNullable(deleted).ifPresent(exitsGroupMember::setDeleted);
       if(Boolean.TRUE.equals(deleted)){
           exitsGroupMember.setDeletedAt(LocalDateTime.now());
       }
       exitsGroupMember.setUpdatedAt(LocalDateTime.now());
        return GroupMemberMapper.toDTO(groupMemberRepository.save(exitsGroupMember));
    }
    @Transactional
    private GroupMemberDTO changeGroupMemberStatusByUserIdAndGroupId(Long userId,Long groupId,Status status,Boolean deleted){
        var groupMember = getGroupMemberEntityByUserIdAndGroupId(userId,groupId);
        if(groupMember.isEmpty()){
            throw new IllegalArgumentException("Group Member Not Exists !!");
        }
        GroupMember exitsGroupMember = groupMember.get();
        Optional.ofNullable(status).ifPresent(exitsGroupMember::setStatus);
        Optional.ofNullable(deleted).ifPresent(exitsGroupMember::setDeleted);
        if(Boolean.TRUE.equals(deleted)){
            exitsGroupMember.setDeletedAt(LocalDateTime.now());
        }
        else {
            exitsGroupMember.setDeletedAt(null);
        }
        exitsGroupMember.setUpdatedAt(LocalDateTime.now());
        return GroupMemberMapper.toDTO(groupMemberRepository.save(exitsGroupMember));
    }
    @CachePut(value = "groupMembersById",key = "#groupMemberId")
    public GroupMemberDTO activeGroupMemberByGroupMemberId(Long groupMemberId){
        var groupMember = getGroupMemberEntityById(groupMemberId).
                orElseThrow(()->new IllegalArgumentException("Group Member Not Found !"));
        if(groupMember.getStatus().equals(Status.ACTIVE)){
            throw new IllegalArgumentException("This Member Already Active !");
        }
        var group = groupRepository.findById(groupMember.getGroup().getId()).
                orElseThrow(()->new IllegalArgumentException("Group Not Found !"));
        group.getMembers().add(groupMember);
        group.setNumberOfMembers(group.getNumberOfMembers().add(BigInteger.ONE));
        groupRepository.save(group);
        return changeGroupMemberStatusByGroupMemberId(groupMemberId,Status.ACTIVE,false);
    }
    @CachePut(value = "groupMembersById",key = "#groupMemberId")
    public GroupMemberDTO inactiveGroupMemberByGroupMemberId(Long groupMemberId){
        var groupMember = getGroupMemberEntityById(groupMemberId).
                orElseThrow(()->new IllegalArgumentException("Group Member Not Found !"));
        if(groupMember.getStatus().equals(Status.INACTIVE)){
            throw new IllegalArgumentException("This Member Already Inactive !");
        }
        var group = groupRepository.findById(groupMember.getGroup().getId()).
                orElseThrow(()->new IllegalArgumentException("Group Not Found !"));
        group.getMembers().remove(groupMember);
        group.setNumberOfMembers(group.getNumberOfMembers().subtract(BigInteger.ONE));
        groupRepository.save(group);
        return changeGroupMemberStatusByGroupMemberId(groupMemberId,Status.INACTIVE,false);
    }
    @CachePut(value = "groupMembersById",key = "#groupMemberId")
    public GroupMemberDTO banGroupMemberByGroupMemberId(Long groupMemberId){
        var groupMember = getGroupMemberEntityById(groupMemberId).
                orElseThrow(()->new IllegalArgumentException("Group Member Not Found !"));
        if(groupMember.getStatus().equals(Status.BANNED)){
            throw new IllegalArgumentException("This Member Already Banned !");
        }
        var group = groupRepository.findById(groupMember.getGroup().getId()).
                orElseThrow(()->new IllegalArgumentException("Group Not Found !"));
        group.getMembers().remove(groupMember);
        group.setNumberOfMembers(group.getNumberOfMembers().subtract(BigInteger.ONE));
        groupRepository.save(group);
        return changeGroupMemberStatusByGroupMemberId(groupMemberId,Status.BANNED,false);
    }
    @CacheEvict(value = "groupMembersById",key = "#groupMemberId")
    public GroupMemberDTO deleteGroupMemberByGroupMemberId(Long groupMemberId){
        var groupMember = getGroupMemberEntityById(groupMemberId).
                orElseThrow(()->new IllegalArgumentException("Group Member Not Found !"));
        if(groupMember.getStatus().equals(Status.DELETED)){
            throw new IllegalArgumentException("This Member Already Deleted !");
        }
        var group = groupRepository.findById(groupMember.getGroup().getId()).
                orElseThrow(()->new IllegalArgumentException("Group Not Found !"));
        group.getMembers().remove(groupMember);
        group.setNumberOfMembers(group.getNumberOfMembers().subtract(BigInteger.ONE));
        groupRepository.save(group);
        return changeGroupMemberStatusByGroupMemberId(groupMemberId,Status.DELETED,true);
    }
    @CachePut(value = "groupMembersByUserAndGroupIDs",key = "#userId + '-' + #groupId")
    public GroupMemberDTO activeGroupMemberByUserIdGroupId(Long userId,Long groupId){
        return changeGroupMemberStatusByUserIdAndGroupId(userId,groupId,Status.ACTIVE,false);
    }
    @CachePut(value = "groupMembersByUserAndGroupIDs",key = "#userId + '-' + #groupId")
    public GroupMemberDTO inactiveGroupMemberByUserIdGroupId(Long userId,Long groupId){
        return changeGroupMemberStatusByUserIdAndGroupId(userId,groupId,Status.INACTIVE,false);
    }
    @CachePut(value = "groupMembersByUserAndGroupIDs",key = "#userId + '-' + #groupId")
    public GroupMemberDTO banGroupMemberByUserIdGroupId(Long userId,Long groupId){
        return changeGroupMemberStatusByUserIdAndGroupId(userId,groupId,Status.BANNED,false);
    }
    @CacheEvict(value = "groupMembersByUserAndGroupIDs",key = "#userId + '-' + #groupId")
    public GroupMemberDTO deleteGroupMemberByUserIdGroupId(Long userId,Long groupId){
        return changeGroupMemberStatusByUserIdAndGroupId(userId,groupId,Status.DELETED,true);
    }
    private Page<GroupMemberDTO> getGroupMembersByStatus(Status status,Pageable pageable){
        return groupMemberRepository.findByStatus(status,pageable).map(GroupMemberMapper::toDTO);
    }

    public Page<GroupMemberDTO> getActiveGroupMembers(Pageable pageable){
        return getGroupMembersByStatus(Status.ACTIVE,pageable);
    }
    public Page<GroupMemberDTO> getInactiveGroupMembers(Pageable pageable){
        return getGroupMembersByStatus(Status.INACTIVE,pageable);
    }
    public Page<GroupMemberDTO> getBannedGroupMembers(Pageable pageable){
        return getGroupMembersByStatus(Status.BANNED,pageable);
    }
    public Page<GroupMemberDTO> getDeletedGroupMembers(Pageable pageable){
        return getGroupMembersByStatus(Status.DELETED,pageable);
    }
    public Page<GroupMemberDTO> getAllUserMembershipsByUserId(Long userId,Pageable pageable){
        return groupMemberRepository.findByUser_Id(userId,pageable).map(GroupMemberMapper::toDTO);
    }
    public Page<GroupMemberDTO> getAllGroupMembersWithoutAnyConditions(Pageable pageable){
        return groupMemberRepository.findAll(pageable).map(GroupMemberMapper::toDTO);
    }
}
