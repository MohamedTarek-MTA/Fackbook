package com.fackbook.React.Repository;

import com.fackbook.React.Entity.React;
import com.fackbook.Request.Enum.RequestTargetType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

public interface ReactRepository extends JpaRepository<React,Long> {
    Optional<React> findByUser_IdAndTargetIdAndTargetType(Long userId, Long targetId, RequestTargetType targetType);

    @Query("""
            Select r.reactType,COUNT(r)
             From React r
             Where r.targetType = :targetType
                And r.targetId = :targetId
            Group By r.reactType
            """)
    List<Object[]> countByTargetGrouped(@Param("targetType") RequestTargetType targetType,
                                         @Param("targetId") Long targetId);
}
