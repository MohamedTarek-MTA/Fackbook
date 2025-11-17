package com.fackbook.React.Entity;

import com.fackbook.React.Enum.ReactType;
import com.fackbook.Report.Enum.Type;
import com.fackbook.User.Entity.User;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "reacts")
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
    private Type targetType;

    @Column(updatable = false)
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;

    @ManyToOne(fetch = FetchType.LAZY,cascade = CascadeType.ALL)
    @JoinColumn(name = "user_id",nullable = false)
    private User user;
}
