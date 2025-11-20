package com.fackbook.Group.Repository;

import com.fackbook.Group.Entity.Group;
import org.springframework.data.jpa.repository.JpaRepository;

public interface GroupRepository extends JpaRepository<Group,Long> {
}
