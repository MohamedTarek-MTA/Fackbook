package com.fackbook.Group.Entity;

import com.fackbook.User.Entity.User;
import com.fackbook.User.Enum.Role;
import com.fackbook.User.Enum.Status;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.util.List;

@Entity
@Table(name = "group_members")
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class GroupMember {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    public Long id;

    @NotNull
    @Enumerated(EnumType.STRING)
    private Status status;
    @NotNull
    @Enumerated(EnumType.STRING)
    public Role role;

    @OneToMany(mappedBy = "group",cascade = CascadeType.ALL,fetch = FetchType.LAZY)
    private List<Group> groups;

    @ManyToOne(cascade = CascadeType.ALL,fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id",nullable = false)
    private User user;
}
