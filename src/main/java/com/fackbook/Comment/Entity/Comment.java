package com.fackbook.Comment.Entity;

import com.fackbook.Post.Entity.Post;
import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Enum.VisibilityStatus;
import com.fackbook.Post.Util.Interface.AccessibleContent;
import com.fackbook.Post.Util.Interface.MediaAttachable;
import com.fackbook.Reply.Entity.Reply;
import com.fackbook.User.Entity.User;
import jakarta.persistence.*;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "comments")
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class Comment implements AccessibleContent, MediaAttachable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;


    private String content;
    private String imageUrl;
    private String videoUrl;

    @Column(updatable = false)
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private LocalDateTime deletedAt;

    private Boolean deleted = false;

    @NotNull
    @Enumerated(EnumType.STRING)
    private VisibilityStatus visibilityStatus;
    @NotNull
    @Enumerated(EnumType.STRING)
    private ModerationStatus moderationStatus;

    @Min(0)
    private BigInteger numberOfReacts = BigInteger.ZERO;
    @Min(0)
    private BigInteger numberOfReplies = BigInteger.ZERO;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "post_id", nullable = false)
    private Post post;

    @OneToMany(mappedBy = "comment", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<Reply> replies;

    @Override
    public Long getAuthorId() {
        return this.user.getId();
    }

    @Override
    public Long getPostAuthorId() {
        return this.post.getUser().getId();
    }

    @Override
    public Long getGroupOwnerId() {
        return this.post.getGroup() != null ? this.post.getGroup().getUser().getId():null;
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
