package com.fackbook.Request.Repository;

import com.fackbook.Request.Entity.Request;
import com.fackbook.Request.Enum.RequestActionType;
import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.Request.Enum.Status;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface RequestRepository extends JpaRepository<Request,Long> {

    Page<Request> findByUser_Id(Long userId, Pageable pageable);

    Page<Request> findByUser_IdAndActionType(Long userId, RequestActionType actionType, Pageable pageable);

    Page<Request> findByTargetIdAndActionType(Long targetId, RequestActionType actionType, Pageable pageable);

    Page<Request> findByTargetIdInAndActionType(List<Long> targetIds, RequestActionType actionType, Pageable pageable);

    Page<Request> findByStatus(Status status,Pageable pageable);
    Page<Request> findByActionType(RequestActionType actionType,Pageable pageable);
    Page<Request> findByTargetType(RequestTargetType targetType,Pageable pageable);
    Optional<Request> findByUser_IdAndTargetIdAndActionType(Long userId,Long targetId,RequestActionType actionType);

    /**
     * Requests visible to a user: sent by them, or where they are receiver (friend/group owner or admin/invited).
     * Caller must pass groupIds = groups where the user is owner or GROUP_ADMIN.
     */
    @Query("""
        SELECT r FROM Request r WHERE
        r.user.id = :userId
        OR (r.actionType = 'FRIENDSHIP_REQUEST' AND r.targetType = 'USER' AND r.targetId = :userId)
        OR (r.actionType = 'GROUP_JOIN_REQUEST' AND r.targetType = 'GROUP' AND r.targetId IN :groupIds)
        OR (r.actionType = 'GROUP_INVITE' AND r.targetType = 'GROUP' AND r.user.id = :userId)
        """)
    Page<Request> findVisibleToUser(@Param("userId") Long userId, @Param("groupIds") List<Long> groupIds, Pageable pageable);
}
