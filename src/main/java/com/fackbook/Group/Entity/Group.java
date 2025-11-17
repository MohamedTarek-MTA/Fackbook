package com.fackbook.Group.Entity;

import com.fackbook.User.Enum.Status;
import com.fackbook.Post.Entity.Post;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "groups",indexes = {
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
    @Column(unique = true,name = "name")
    private String name;
    private String description;
    private String imageUrl;

    @Column(updatable = false)
    private LocalDateTime createdAt;
    private LocalDateTime deletedAt;
    private LocalDateTime updatedAt;

    @NotNull
    @Enumerated(EnumType.STRING)
    private Status status;

    @OneToMany(mappedBy = "group",cascade = CascadeType.ALL,fetch = FetchType.LAZY)
    private List<Post> posts;
}
