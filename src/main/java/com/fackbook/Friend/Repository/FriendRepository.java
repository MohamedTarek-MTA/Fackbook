package com.fackbook.Friend.Mapper;

import com.fackbook.Friend.Entity.Friend;
import org.springframework.data.jpa.repository.JpaRepository;

public interface FriendRepository extends JpaRepository<Friend,Long> {
}
