package com.fackbook.Reply.Entity;

import com.fackbook.Comment.Entity.Comment;
import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Enum.VisibilityStatus;
import com.fackbook.Post.Util.Interface.AccessibleContent;
import com.fackbook.Post.Util.Interface.MediaAttachable;
import com.fackbook.User.Entity.User;
import jakarta.persistence.*;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.math.BigInteger;
import java.time.LocalDateTime;

@Entity
@Table(name = "replies")
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class Reply implements AccessibleContent, MediaAttachable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;


    private String content;
    private String imageUrl;
    private String videoUrl;

    @NotNull
    @Enumerated(EnumType.STRING)
    private VisibilityStatus visibilityStatus;
    @NotNull
    @Enumerated(EnumType.STRING)
    private ModerationStatus moderationStatus;
    @Min(0)
    private BigInteger numberOfReacts = BigInteger.ZERO;

    @Column(updatable = false)
    private LocalDateTime createdAt;
    private LocalDateTime deletedAt;
    private LocalDateTime updatedAt;

    private Boolean deleted = false;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "comment_id", nullable = false)
    private Comment comment;

    @Override
    public Long getAuthorId() {
        return this.user.getId();
    }

    @Override
    public Long getPostAuthorId() {
        return this.comment.getPost().getUser().getId();
    }

    @Override
    public Long getGroupOwnerId() {
        return this.comment.getPost().getGroup() != null ?
                this.comment.getPost().getGroup().getUser().getId():null;
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
