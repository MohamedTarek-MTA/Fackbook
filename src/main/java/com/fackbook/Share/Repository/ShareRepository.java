package com.fackbook.Share.Repository;

import com.fackbook.Share.Entity.Share;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ShareRepository extends JpaRepository<Share,Long> {
    Page<Share> findByUser_Id(Long userId, Pageable pageable);
    Page<Share> findByPost_Id(Long postId,Pageable pageable);
}
