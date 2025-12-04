package com.fackbook.Post.Repository;

import com.fackbook.Post.Entity.Post;
import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Enum.Privacy;
import com.fackbook.Post.Enum.VisibilityStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface PostRepository extends JpaRepository<Post,Long> {
    Page<Post> findByUser_Id(Long userId, Pageable pageable);
    Page<Post> findByGroup_Id(Long groupId,Pageable pageable);
    Page<Post> findByPrivacy(Privacy privacy,Pageable pageable);
    Page<Post> findByVisibilityStatus(VisibilityStatus visibilityStatus, Pageable pageable);
    Page<Post> findByModerationStatus(ModerationStatus moderationStatus, Pageable pageable);
    Page<Post> findByGroup_NameIgnoreCase(String name,Pageable pageable);
    Page<Post> findByUser_IdAndGroup_Id(Long userId, Long groupId, Pageable pageable);
    Page<Post> findByUser_NameContainingIgnoreCase(String name,Pageable pageable);
}
