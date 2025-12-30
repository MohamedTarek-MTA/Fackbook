package com.fackbook.Notification;

import com.fackbook.Comment.Repository.CommentRepository;
import com.fackbook.Group.Entity.Group;
import com.fackbook.Group.Repository.GroupRepository;
import com.fackbook.Post.Repository.PostRepository;
import com.fackbook.React.Entity.React;
import com.fackbook.Reply.Repository.ReplyRepository;
import com.fackbook.Request.Entity.Request;
import lombok.RequiredArgsConstructor;
import org.checkerframework.checker.units.qual.C;
import org.springframework.stereotype.Service;

import java.time.Instant;

@Service
@RequiredArgsConstructor
public class NotificationService {
    private final GroupRepository groupRepository;
    private final PostRepository postRepository;
    private final CommentRepository commentRepository;
    private final ReplyRepository replyRepository;
    private final NotificationProducer notificationProducer;
    private Group getGroup(Long groupId) {
        return groupRepository.findById(groupId)
                .orElseThrow();
    }
    private String getPostOwnerId(Long postId) {
        return postRepository.findById(postId)
                .map(post -> post.getUser().getId().toString())
                .orElseThrow();
    }
    private String getCommentOwnerId(Long commentId){
        return commentRepository.findById(commentId).map(
                comment -> comment.getUser().getId().toString()
        ).orElseThrow();
    }
    private String getReplyOwnerId(Long replyId){
        return replyRepository.findById(replyId).map(
                reply -> reply.getUser().getId().toString()
        ).orElseThrow();
    }
    public void sendNotificationViaReact(React react){
        Notification notification = switch (react.getTargetType()){
            case POST -> Notification.builder()
                    .userId(getPostOwnerId(react.getTargetId()))
                    .fromUserId(react.getUser().getId().toString())
                    .read(false)
                    .createdAt(Instant.now())
                    .message(react.getUser().getName()+" Reacted On Your Post !")
                    .build();

            case COMMENT -> Notification.builder()
                    .userId(getCommentOwnerId(react.getTargetId()))
                    .fromUserId(react.getUser().getId().toString())
                    .read(false)
                    .createdAt(Instant.now())
                    .message(react.getUser().getName()+" Reacted On Your Comment !")
                    .build();

            case REPLY -> Notification.builder()
                    .userId(getReplyOwnerId(react.getTargetId()))
                    .fromUserId(react.getUser().getId().toString())
                    .read(false)
                    .createdAt(Instant.now())
                    .message(react.getUser().getName()+" Reacted On Your Reply !")
                    .build();
            default -> null;
        };
        if (notification != null) {
            notificationProducer.sendNotification(notification);
        }
    }
    public void sendNotificationViaRequest(Request request){
        Notification notification = switch (request.getActionType()) {

            case FRIENDSHIP_REQUEST ->  Notification.builder()
                    .userId(request.getTargetId().toString())
                    .targetType(request.getTargetType().name())
                    .read(false)
                    .fromUserId(request.getUser().getId().toString())
                    .message(request.getUser().getName() + " sent you a friend request")
                    .createdAt(Instant.now())
                    .build();


            case GROUP_JOIN_REQUEST ->  Notification.builder()
                    .userId(getGroup(request.getTargetId()).getUser().getId().toString())
                    .targetType(request.getTargetType().name())
                    .read(false)
                    .fromUserId(request.getUser().getId().toString())
                    .message(request.getUser().getName() + " requested to join your group")
                    .read(false)
                    .createdAt(Instant.now())
                    .build();

            case GROUP_INVITE ->  Notification.builder()
                    .userId(request.getUser().getId().toString())
                    .targetType(request.getTargetType().name())
                    .read(false)
                    .fromUserId(getGroup(request.getTargetId()).getUser().getId().toString())
                    .message(getGroup(request.getTargetId()).getUser().getName()+" invited you to a group "+getGroup(request.getTargetId()).getName())
                    .build();

            case CONTENT_APPROVAL ->  Notification.builder()
                    .userId(getPostOwnerId(request.getTargetId()))
                    .targetType(request.getTargetType().name())
                    .fromUserId(request.getUser().getId().toString())
                    .read(false)
                    .createdAt(Instant.now())
                    .message("Your Post Has Been "+request.getStatus())
                    .build();
            default -> null;
        };

        if (notification != null) {
            notificationProducer.sendNotification(notification);
        }
    }
}
