package com.fackbook.Share.Repository;

import com.fackbook.Share.Entity.Share;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ShareRepository extends JpaRepository<Share,Long> {
}
