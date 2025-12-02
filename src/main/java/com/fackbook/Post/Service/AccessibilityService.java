package com.fackbook.Post.Service;

import com.fackbook.Post.Interface.AccessibleContent;
import com.fackbook.User.Enum.Role;
import com.fackbook.User.Service.UserService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class AccessibilityService {
    private final UserService userService;

    public void validateVisibility(AccessibleContent content, Long userId) {

        Long authorId = content.getAuthorId();
        Long postAuthorId = content.getPostAuthorId();
        Long groupOwnerId = content.getGroupOwnerId();
        var user = userService.getUserEntityById(userId);

        boolean isAuthor = authorId.equals(userId);
        boolean isPostAuthor = postAuthorId.equals(userId);

        boolean isSystemAdmin = user.getRole().equals(Role.SYSTEM_ADMIN);
        boolean isGroupAdmin = groupOwnerId != null
                && groupOwnerId.equals(userId)
                && user.getRole().equals(Role.GROUP_ADMIN);

        switch (content.getVisibilityStatus()) {
            case ACTIVE:
                return;

            case HIDDEN:
                if (isAuthor || isPostAuthor || isSystemAdmin || isGroupAdmin) return;
                break;

            case DELETED:
                if (isSystemAdmin) return;
                break;

            case REMOVED_BY_ADMIN:
                if (isSystemAdmin || isGroupAdmin) return;
                break;

            default:
                throw new IllegalArgumentException("Unknown visibility status: " + content.getVisibilityStatus());
        }

        throw new IllegalArgumentException("You cannot access this content.");
    }

    public void validateModeration(AccessibleContent content, Long userId) {

        Long authorId = content.getAuthorId();
        Long groupOwnerId = content.getGroupOwnerId();
        var user = userService.getUserEntityById(userId);

        boolean isAuthor = authorId.equals(userId);
        boolean isSystemAdmin = user.getRole().equals(Role.SYSTEM_ADMIN);
        boolean isGroupAdmin = groupOwnerId != null
                && groupOwnerId.equals(userId)
                && user.getRole().equals(Role.GROUP_ADMIN);

        if (isAuthor || isSystemAdmin || isGroupAdmin) return;

        switch (content.getModerationStatus()) {
            case NONE:
                return;

            case PENDING_APPROVAL:
            case REJECTED:
            case REPORTED:
            case UNDER_REVIEW:
                throw new IllegalArgumentException("Content is under moderation and cannot be accessed.");

            default:
                throw new IllegalArgumentException("Unknown moderation status: " + content.getModerationStatus());
        }
    }
}
