package com.fackbook.Request.Repository;

import com.fackbook.Request.Entity.Request;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RequestRepository extends JpaRepository<Request,Long> {
}
