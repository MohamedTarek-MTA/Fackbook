package com.fackbook.React.Entity;

import com.fackbook.React.Enum.ReactType;
import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.User.Entity.User;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "reacts",uniqueConstraints = {
        @UniqueConstraint(
                columnNames = {"user_id", "target_type", "target_id"}
        )
})
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class React {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @NotNull
    private Long targetId;

    @NotNull
    @Enumerated(EnumType.STRING)
    private ReactType reactType;

    @NotNull
    @Enumerated(EnumType.STRING)
    private RequestTargetType targetType;

    @Column(updatable = false)
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;

    private Boolean deleted = false;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;
}
