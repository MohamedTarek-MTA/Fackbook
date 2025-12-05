package com.fackbook.Report.Repository;

import com.fackbook.Report.Entity.Report;
import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.Request.Enum.Status;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;


public interface ReportRepository extends JpaRepository<Report,Long> {
    Page<Report> findByUser_Id(Long userId, Pageable pageable);
    Page<Report> findByTargetIdAndTargetType(Long targetId, RequestTargetType targetType,Pageable pageable);
    Page<Report> findByReviewerId(Long reviewerId,Pageable pageable);
    Page<Report> findByUser_IdAndTargetIdAndTargetType(Long userId,Long targetId,RequestTargetType targetType,Pageable pageable);
    Page<Report> findByStatus(Status status,Pageable pageable);
}
