package com.fackbook.Friend.Repository;

import com.fackbook.Friend.Entity.Friendship;
import com.fackbook.Friend.Enum.Status;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface FriendshipRepository extends JpaRepository<Friendship,Long> {

    Page<Friendship> findByUser_IdAndStatus(Long userId,Status status,Pageable pageable);
    Page<Friendship> findByFriend_idAndStatus(Long friendId,Status status,Pageable pageable);
    Page<Friendship> findByStatus(Status status,Pageable pageable);
    Optional<Friendship> findByUser_IdAndFriend_Id(Long userId,Long friendId);
}
