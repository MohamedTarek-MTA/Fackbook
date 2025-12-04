package com.fackbook.Comment.Repository;

import com.fackbook.Comment.Entity.Comment;
import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Enum.VisibilityStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface CommentRepository extends JpaRepository<Comment,Long> {
    Page<Comment> findByPost_Id(Long postId, Pageable pageable);
    Page<Comment> findByUser_Id(Long userId,Pageable pageable);
    Optional<Comment> findByUser_IdAndPost_Id(Long userId,Long postId);
    Page<Comment> findByModerationStatus(ModerationStatus moderationStatus, Pageable pageable);
    Page<Comment> findByVisibilityStatus(VisibilityStatus visibilityStatus,Pageable pageable);
    Page<Comment> findByUser_NameContainingIgnoreCase(String name,Pageable pageable);
}
