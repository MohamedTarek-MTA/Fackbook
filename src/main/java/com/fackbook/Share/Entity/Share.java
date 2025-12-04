package com.fackbook.Share.Entity;

import com.fackbook.Post.Entity.Post;
import com.fackbook.User.Entity.User;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "shares")
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class Share {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String content;


    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "post_id", nullable = false)
    private Post post;
    @Column(updatable = false)
    private LocalDateTime createdAt;
}
