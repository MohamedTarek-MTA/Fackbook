package com.fackbook.Request.Repository;

import com.fackbook.Request.Entity.Request;
import com.fackbook.Request.Enum.RequestActionType;
import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.Request.Enum.Status;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface RequestRepository extends JpaRepository<Request,Long> {

    Page<Request> findByUser_Id(Long userId, Pageable pageable);

    Page<Request> findByTargetIdAndActionType(Long targetId, RequestActionType actionType, Pageable pageable);

    Page<Request> findByStatus(Status status,Pageable pageable);
    Page<Request> findByActionType(RequestActionType actionType,Pageable pageable);
    Page<Request> findByTargetType(RequestTargetType targetType,Pageable pageable);
    Optional<Request> findByUser_IdAndTargetIdAndActionType(Long userId,Long targetId,RequestActionType actionType);
}
