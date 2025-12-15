package com.fackbook.React.Service;

import com.fackbook.Comment.Repository.CommentRepository;
import com.fackbook.Post.Enum.VisibilityStatus;
import com.fackbook.Post.Repository.PostRepository;
import com.fackbook.React.Entity.React;
import com.fackbook.React.Enum.ReactType;
import com.fackbook.React.Repository.ReactRepository;
import com.fackbook.Reply.Repository.ReplyRepository;
import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.User.Repository.UserRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class ReactService {
    private final UserRepository userRepository;
    private final PostRepository postRepository;
    private final CommentRepository commentRepository;
    private final ReplyRepository replyRepository;
    private final ReactRepository reactRepository;

    private Long resolveTargetId(RequestTargetType targetType,Long targetId){
        return switch (targetType){
            case POST -> {
                var post = postRepository.findById(targetId).orElseThrow(()->new IllegalArgumentException("Post Not Found !"));
                if(post.getVisibilityStatus().equals(VisibilityStatus.DELETED)||post.getVisibilityStatus().equals(VisibilityStatus.REMOVED_BY_ADMIN))
                    throw new IllegalArgumentException("Post is already "+post.getVisibilityStatus().name());
                yield post.getId();
            }
            case COMMENT -> {
                var comment = commentRepository.findById(targetId).orElseThrow(()->new IllegalArgumentException("Comment Not Found !"));
                if(comment.getVisibilityStatus().equals(VisibilityStatus.DELETED)||comment.getVisibilityStatus().equals(VisibilityStatus.REMOVED_BY_ADMIN))
                    throw new IllegalArgumentException("Comment is already "+comment.getVisibilityStatus().name());
                yield comment.getId();
            }
            default -> throw new IllegalArgumentException("Unknown target type "+targetType);
        };
    }
    @Transactional
    private void changeNumberOfReactsByOne(Long actualTargetId,RequestTargetType targetType,boolean increased){
        switch (targetType){
            case POST -> {
                var post = postRepository.findById(actualTargetId).get();
                if(Boolean.TRUE.equals(increased)){
                    post.setNumberOfReacts(post.getNumberOfReacts().add(BigInteger.ONE));
                }
                if(Boolean.FALSE.equals(increased)){
                    var current = post.getNumberOfReacts();
                    if(current.compareTo(BigInteger.ZERO) > 0){
                        post.setNumberOfReacts(post.getNumberOfReacts().subtract(BigInteger.ONE));
                    }
                }
                postRepository.save(post);
            }
            case COMMENT -> {
                var comment = commentRepository.findById(actualTargetId).get();
                if(Boolean.TRUE.equals(increased)){
                    comment.setNumberOfReacts(comment.getNumberOfReacts().add(BigInteger.ONE));
                }
                if(Boolean.FALSE.equals(increased)){
                    var current = comment.getNumberOfReacts();
                    if(current.compareTo(BigInteger.ZERO) > 0){
                        comment.setNumberOfReacts(comment.getNumberOfReacts().subtract(BigInteger.ONE));
                    }                }
                commentRepository.save(comment);
            }
        }
    }
    @Transactional
    public void react(Long userId, Long targetId, RequestTargetType targetType, ReactType reactType){
        Long actualTargetId = resolveTargetId(targetType,targetId);
        var existedReact = reactRepository.findByUser_IdAndTargetIdAndTargetType(userId,actualTargetId,targetType);
        if(existedReact.isPresent()){
            React r = existedReact.get();
            if(r.getReactType().equals(reactType)){
                reactRepository.delete(r);
                changeNumberOfReactsByOne(actualTargetId,targetType,false);
            }
            else {
                r.setReactType(r.getReactType());
            }
        }
        else {
            React react = React.builder()
                    .user(userRepository.findById(userId).orElseThrow(() -> new IllegalArgumentException("User Not Found !")))
                    .targetId(actualTargetId)
                    .targetType(targetType)
                    .reactType(reactType)
                    .createdAt(LocalDateTime.now())
                    .build();
            reactRepository.save(react);
            changeNumberOfReactsByOne(actualTargetId, targetType, true);

        }
    }
    @Transactional(readOnly = true)
    public Map<ReactType,Long> getReactionSummary(Long targetId,RequestTargetType targetType){
        Long actualTargetId = resolveTargetId(targetType,targetId);

        List<Object[]> rows = reactRepository.countByTargetGrouped(targetType,actualTargetId);

        Map<ReactType,Long> summary = new EnumMap<>(ReactType.class);

        for(Object[] row : rows){
            ReactType reactType = (ReactType) row[0];
            Long count = (Long) row[1];
            summary.put(reactType,count);
        }
        return summary;
    }
}
