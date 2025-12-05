package com.fackbook.Group.Entity;

import com.fackbook.Group.Enum.ApprovalMode;
import com.fackbook.Group.Enum.JoinPolicy;
import com.fackbook.User.Entity.User;
import com.fackbook.User.Enum.Status;
import com.fackbook.Post.Entity.Post;
import jakarta.persistence.*;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "`groups`", indexes = {
        @Index(name = "idx_group_name", columnList = "name")
})
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class Group {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @NotBlank
    @Column(unique = true, name = "name")
    private String name;
    private String description;
    private String imageUrl;

    @Min(0)
    private BigInteger numberOfMembers = BigInteger.ZERO;

    @Column(updatable = false)
    private LocalDateTime createdAt;
    private LocalDateTime deletedAt;
    private LocalDateTime updatedAt;

    private Boolean deleted = false;

    @NotNull
    @Enumerated(EnumType.STRING)
    private Status status;
    @NotNull
    @Enumerated(EnumType.STRING)
    private ApprovalMode approvalMode;
    @NotNull
    @Enumerated(EnumType.STRING)
    private JoinPolicy joinPolicy;

    @OneToMany(mappedBy = "group", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<Post> posts;

    @OneToMany(mappedBy = "group", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<GroupMember> members;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id",nullable = false)
    private User user;
}
