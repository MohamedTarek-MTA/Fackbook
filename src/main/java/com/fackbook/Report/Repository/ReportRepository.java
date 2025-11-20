package com.fackbook.Report.Repository;

import com.fackbook.Report.Entity.Report;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ReportRepository extends JpaRepository<Report,Long> {
}
