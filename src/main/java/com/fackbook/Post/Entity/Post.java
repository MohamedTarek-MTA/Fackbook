package com.fackbook.Post.Entity;

import com.fackbook.Comment.Entity.Comment;
import com.fackbook.Group.Entity.Group;
import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Enum.Privacy;
import com.fackbook.Post.Enum.VisibilityStatus;
import com.fackbook.Post.Util.Interface.AccessibleContent;
import com.fackbook.Post.Util.Interface.MediaAttachable;
import com.fackbook.Share.Entity.Share;
import com.fackbook.User.Entity.User;
import jakarta.persistence.*;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "posts")
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class Post implements AccessibleContent, MediaAttachable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;


    private String content;
    private String imageUrl;
    private String videoUrl;

    @Min(0)
    private BigInteger numberOfReacts = BigInteger.ZERO;
    @Min(0)
    private BigInteger numberOfComments = BigInteger.ZERO;
    @Min(0)
    private BigInteger numberOfShares = BigInteger.ZERO;

    @NotNull
    @Enumerated(EnumType.STRING)
    private Privacy privacy;

    @NotNull
    @Enumerated(EnumType.STRING)
    private VisibilityStatus visibilityStatus;
    @NotNull
    @Enumerated(EnumType.STRING)
    private ModerationStatus moderationStatus;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;

    private Boolean deleted = false;

    @OneToMany(mappedBy = "post", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<Comment> comments;

    @OneToMany(mappedBy = "post", fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    private List<Share> shares;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;


    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "group_id", nullable = true)
    private Group group;

    @Override
    public Long getAuthorId() {
        return this.user.getId();
    }

    @Override
    public Long getPostAuthorId() {
        return this.user.getId();
    }

    @Override
    public Long getGroupOwnerId() {
        return this.group != null ? this.group.getUser().getId(): null;
    }
    @Override
    public VisibilityStatus getVisibilityStatus(){
        return this.visibilityStatus;
    }
    @Override
    public ModerationStatus getModerationStatus(){
        return this.moderationStatus;
    }
    @Override
    public String getImageUrl(){
        return imageUrl;
    }
    @Override
    public void setImageUrl(String url){
        this.imageUrl=url;
    }
    @Override
    public String getVideoUrl(){
        return videoUrl;
    }
    @Override
    public void setVideoUrl(String url){
        this.videoUrl=url;
    }
}
