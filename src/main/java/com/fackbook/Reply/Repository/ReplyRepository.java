package com.fackbook.Reply.Repository;

import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Enum.VisibilityStatus;
import com.fackbook.Reply.Entity.Reply;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ReplyRepository extends JpaRepository<Reply,Long> {

    Page<Reply> findByComment_Id(Long commentId, Pageable pageable);
    Page<Reply> findByUser_Id(Long userId,Pageable pageable);
    Page<Reply> findByUser_NameContainingIgnoreCase(String name,Pageable pageable);
    Page<Reply> findByComment_Post_Id(Long postId,Pageable pageable);
    Page<Reply> findByVisibilityStatus(VisibilityStatus visibilityStatus,Pageable pageable);
    Page<Reply> findByModerationStatus(ModerationStatus moderationStatus,Pageable pageable);
}
