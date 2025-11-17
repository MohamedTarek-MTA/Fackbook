package com.fackbook.Report.Entity;

import com.fackbook.Report.Enum.Status;
import com.fackbook.Report.Enum.Type;
import com.fackbook.User.Entity.User;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "reports")
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class Report {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @NotNull
    private Long targetId;

    @NotBlank
    private String content;
    private String imageUrl;
    private String videoUrl;

    @Column(updatable = false)
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;

    @NotNull
    @Enumerated(EnumType.STRING)
    private Type type;
    @NotNull
    @Enumerated(EnumType.STRING)
    private Status status;

    @ManyToOne(fetch = FetchType.LAZY,cascade = CascadeType.ALL)
    @JoinColumn(name = "user_id",nullable = false)
    private User user;
}
