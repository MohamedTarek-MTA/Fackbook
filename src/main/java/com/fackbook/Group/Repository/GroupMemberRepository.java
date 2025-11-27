package com.fackbook.Group.Repository;

import com.fackbook.Group.Entity.GroupMember;
import com.fackbook.User.Enum.Role;
import com.fackbook.User.Enum.Status;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface GroupMemberRepository extends JpaRepository<GroupMember,Long> {
    Page<GroupMember> findByGroup_IdAndRole(Long groupId,Role role,Pageable pageable);
    Page<GroupMember> findByGroup_NameIgnoreCase(String groupName,Pageable pageable);
    Page<GroupMember> findByUser_Id(Long userId, Pageable pageable);
    Page<GroupMember> findByRole(Role role,Pageable pageable);
    Page<GroupMember> findByGroup_NameIgnoreCaseAndUser_NameContainingIgnoreCase(String groupName,String userName,Pageable pageable);
    Page<GroupMember> findByStatus(Status status,Pageable pageable);
    Optional<GroupMember> findByUser_IdAndGroup_Id(Long userId,Long groupId);
}
