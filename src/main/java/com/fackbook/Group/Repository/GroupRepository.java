package com.fackbook.Group.Repository;

import com.fackbook.Group.Entity.Group;
import com.fackbook.User.Enum.Status;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface GroupRepository extends JpaRepository<Group,Long> {
    Optional<Group> findByNameIgnoreCase(String name);
    Page<Group> findByStatus(Status status, Pageable pageable);
    Page<Group> findByUser_Id(Long userId,Pageable pageable);

    boolean existsByNameIgnoreCaseAndIdNot(String name , Long id);
}
