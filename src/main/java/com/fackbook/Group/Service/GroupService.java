package com.fackbook.Group.Service;

import com.fackbook.Exception.ImageUploadException;
import com.fackbook.Group.DTO.GroupDTO;
import com.fackbook.Group.Entity.Group;
import com.fackbook.Group.Entity.GroupMember;
import com.fackbook.Group.Enum.ApprovalMode;
import com.fackbook.Group.Enum.JoinPolicy;
import com.fackbook.Group.Mapper.GroupMapper;
import com.fackbook.Group.Repository.GroupRepository;
import com.fackbook.Shared.Helper.FileHelper;
import com.fackbook.Shared.Image.UploadImageService;
import com.fackbook.User.Enum.Role;
import com.fackbook.User.Enum.Status;
import com.fackbook.User.Service.UserService;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Optional;

@Service
@RequiredArgsConstructor
public class GroupService {
    private final GroupRepository groupRepository;
    private final GroupMemberService groupMemberService;
    private final UserService userService;
    private final FileHelper fileHelper;

    public Group getGroupEntityByGroupId(Long groupId){
        return groupRepository.findById(groupId).orElseThrow(()->new IllegalArgumentException("Group Not Found !"));
    }
    public Page<GroupDTO> getGroupsByUserId(Long userId, Pageable pageable){
        return groupRepository.findByUser_Id(userId,pageable).map(GroupMapper::toDTO);
    }

    public Page<GroupDTO> getGroupsByStatus(Status status,Pageable pageable){
        return groupRepository.findByStatus(status,pageable).map(GroupMapper::toDTO);
    }
    @CachePut(value = "groupsByName",key = "#groupName")
    public GroupDTO getGroupByName(String groupName){
        return GroupMapper.toDTO(
                groupRepository.findByNameIgnoreCase(groupName).
                        orElseThrow(()->new IllegalArgumentException("Group Not Found !"))
        );
    }
    @Transactional
    public GroupDTO createNewGroup(Long userId,GroupDTO dto){
        var user = userService.getUserEntityById(userId);
       if(groupRepository.findByNameIgnoreCase(dto.getName()).isPresent()){
           throw new IllegalArgumentException("Sorry This Name Already Used Please Choose Another One !!");
       }
        Group group = Group.builder()
                .user(user)
                .name(dto.getName())
                .description(dto.getDescription())
                .numberOfMembers(BigInteger.ONE)
                .createdAt(LocalDateTime.now())
                .joinPolicy(dto.getJoinPolicy() != null ? dto.getJoinPolicy() :JoinPolicy.PUBLIC)
                .approvalMode(dto.getApprovalMode() != null ? dto.getApprovalMode() : ApprovalMode.NONE)
                .deleted(false)
                .deletedAt(null)
                .imageUrl(null)
                .members(new ArrayList<GroupMember>())
                .status(dto.getStatus())
                .build();
        groupRepository.save(group);
        var groupMember = GroupMember.builder()
                .user(user)
                .role(Role.GROUP_ADMIN)
                .group(group)
                .createdAt(LocalDateTime.now())
                .status(Status.ACTIVE)
                .deletedAt(null)
                .deleted(false)
                .build();
        group.getMembers().add(groupMember);
        return GroupMapper.toDTO(groupRepository.save(group));
    }
    @Transactional
    @CachePut(value = "groupsById",key = "#groupId")
    public GroupDTO updateGroupImage(Long groupId, MultipartFile image){
        var group = getGroupEntityByGroupId(groupId);
        group.setImageUrl(fileHelper.generateImageUrl(image));
        group.setUpdatedAt(LocalDateTime.now());
        return GroupMapper.toDTO(groupRepository.save(group));
    }
    @Transactional
    private GroupDTO changeGroupStatusByGroupId(Long groupId,Status status,Boolean deleted){
        var group = getGroupEntityByGroupId(groupId);
        Optional.ofNullable(status).ifPresent(group::setStatus);
        Optional.ofNullable(deleted).ifPresent(group::setDeleted);
        if(Boolean.TRUE.equals(deleted)){
            group.setDeletedAt(LocalDateTime.now());
        }
        else {
            group.setDeletedAt(null);
        }
        group.setUpdatedAt(LocalDateTime.now());
        return GroupMapper.toDTO(groupRepository.save(group));
    }
    @CachePut(value = "groupsById",key = "#groupId")
    public GroupDTO activeGroup(Long groupId){
        return changeGroupStatusByGroupId(groupId,Status.ACTIVE,false);
    }
    @CachePut(value = "groupsById",key = "#groupId")
    public GroupDTO inactiveGroup(Long groupId){
        return changeGroupStatusByGroupId(groupId,Status.INACTIVE,false);
    }
    @CachePut(value = "groupsById",key = "#groupId")
    public GroupDTO banGroup(Long groupId){
        return changeGroupStatusByGroupId(groupId,Status.BANNED,false);
    }
    @CacheEvict(value = "groupsById",key = "#groupId")
    public GroupDTO deleteGroup(Long groupId){
        return changeGroupStatusByGroupId(groupId,Status.DELETED,true);
    }
    @Transactional
    @CachePut(value = "groupsById",key = "#groupId")
    public GroupDTO updateGroup(Long groupId,GroupDTO dto){
        var group = getGroupEntityByGroupId(groupId);
        if(groupRepository.existsByNameIgnoreCaseAndIdNot(dto.getName(),groupId)){
            throw new IllegalArgumentException("Sorry This Name Already Used Please Choose Another One !!");
        }
        Optional.ofNullable(dto.getName()).ifPresent(group::setName);
        Optional.ofNullable(dto.getDescription()).ifPresent(group::setDescription);
        Optional.ofNullable(dto.getJoinPolicy()).ifPresent(group::setJoinPolicy);
        Optional.ofNullable(dto.getApprovalMode()).ifPresent(group::setApprovalMode);
        group.setUpdatedAt(LocalDateTime.now());
        return GroupMapper.toDTO(groupRepository.save(group));
    }
    public Page<GroupDTO> getAllGroups(Pageable pageable){
        return groupRepository.findAll(pageable).map(GroupMapper::toDTO);
    }
}
