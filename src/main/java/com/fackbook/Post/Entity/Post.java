package com.fackbook.Post.Entity;

import com.fackbook.Comment.Entity.Comment;
import com.fackbook.Group.Entity.Group;
import com.fackbook.Post.Enum.Privacy;
import com.fackbook.Post.Enum.Status;
import com.fackbook.Share.Entity.Share;
import com.fackbook.User.Entity.User;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "posts")
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class Post {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @NotBlank
    private String content;
    private String imageUrl;
    private String videoUrl;

    @NotNull
    @Enumerated(EnumType.STRING)
    private Privacy privacy;
    @NotNull
    @Enumerated(EnumType.STRING)
    private Status status;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;

    @OneToMany(mappedBy = "post",cascade = CascadeType.ALL,fetch = FetchType.LAZY)
    private List<Comment> comments;
    @OneToMany(mappedBy = "post",fetch = FetchType.LAZY,cascade = CascadeType.ALL)
    private List<Share> shares;

    @ManyToOne(fetch = FetchType.LAZY,cascade = CascadeType.ALL)
    @JoinColumn(name = "user_id",nullable = false)
    private User user;
    @ManyToOne(fetch = FetchType.LAZY,cascade = CascadeType.ALL)
    @JoinColumn(name = "group_id",nullable = false)
    private Group group;
}
