package com.fackbook.Reply.Repository;

import com.fackbook.Reply.Entity.Reply;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ReplyRepository extends JpaRepository<Reply,Long> {
}
