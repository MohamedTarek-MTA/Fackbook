package com.fackbook.Post.Interface;

import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Enum.VisibilityStatus;

public interface AccessibleContent {
    Long getAuthorId();
    Long getPostAuthorId();
    Long getGroupOwnerId();
    VisibilityStatus getVisibilityStatus();
    ModerationStatus getModerationStatus();
}
