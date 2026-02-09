package com.fackbook;

import com.fackbook.Friend.Entity.Friendship;
import com.fackbook.Group.Entity.Group;
import com.fackbook.Group.Entity.GroupMember;
import com.fackbook.Group.Enum.ApprovalMode;
import com.fackbook.Group.Enum.JoinPolicy;
import com.fackbook.Post.Entity.Post;
import com.fackbook.Post.Enum.ModerationStatus;
import com.fackbook.Post.Enum.Privacy;
import com.fackbook.Post.Enum.VisibilityStatus;
import com.fackbook.Request.Entity.Request;
import com.fackbook.Request.Enum.RequestActionType;
import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.Share.Entity.Share;
import com.fackbook.User.Entity.User;
import com.fackbook.User.Enum.Gender;
import com.fackbook.User.Enum.Role;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * Shared test data for service unit tests.
 */
public final class TestFixtures {

    private TestFixtures() {}

    // ---------- User ----------
    public static User user(long id, String name, String email, String phone, com.fackbook.User.Enum.Status status, Role role) {
        User u = new User();
        u.setId(id);
        u.setName(name);
        u.setEmail(email);
        u.setPhone(phone);
        u.setPassword("encoded");
        u.setStatus(status);
        u.setRole(role);
        u.setGender(Gender.MALE);
        u.setBirthdate(LocalDate.of(1990, 1, 1));
        u.setEnabled(true);
        u.setDeleted(false);
        u.setCreatedAt(LocalDateTime.now());
        return u;
    }

    public static User user(long id) {
        return user(id, "Test User", "user@test.com", "+1234567890", com.fackbook.User.Enum.Status.ACTIVE, Role.USER);
    }

    public static User groupOwner(long id) {
        return user(id, "Group Owner", "owner@test.com", "+1111111111", com.fackbook.User.Enum.Status.ACTIVE, Role.GROUP_ADMIN);
    }

    // ---------- Group ----------
    public static Group group(long id, String name, User owner, com.fackbook.User.Enum.Status status, JoinPolicy joinPolicy) {
        Group g = new Group();
        g.setId(id);
        g.setName(name);
        g.setDescription("Test group");
        g.setUser(owner);
        g.setStatus(status);
        g.setJoinPolicy(joinPolicy);
        g.setApprovalMode(ApprovalMode.NONE);
        g.setNumberOfMembers(java.math.BigInteger.ONE);
        g.setDeleted(false);
        g.setCreatedAt(LocalDateTime.now());
        g.setMembers(new ArrayList<>());
        return g;
    }

    public static Group group(long id, User owner) {
        return group(id, "Test Group", owner, com.fackbook.User.Enum.Status.ACTIVE, JoinPolicy.PUBLIC);
    }

    // ---------- GroupMember ----------
    public static GroupMember groupMember(long id, User user, Group group, Role role, com.fackbook.User.Enum.Status status) {
        GroupMember gm = new GroupMember();
        gm.setId(id);
        gm.setUser(user);
        gm.setGroup(group);
        gm.setRole(role);
        gm.setStatus(status);
        gm.setDeleted(false);
        gm.setCreatedAt(LocalDateTime.now());
        return gm;
    }

    // ---------- Request ----------
    public static Request request(long id, User user, long targetId, RequestTargetType targetType,
                                  RequestActionType actionType, com.fackbook.Request.Enum.Status status) {
        Request r = new Request();
        r.setId(id);
        r.setUser(user);
        r.setTargetId(targetId);
        r.setTargetType(targetType);
        r.setActionType(actionType);
        r.setStatus(status);
        r.setDeleted(false);
        r.setCreatedAt(LocalDateTime.now());
        return r;
    }

    // ---------- Friendship ----------
    public static Friendship friendship(long id, User user, User friend, com.fackbook.Friend.Enum.Status status) {
        Friendship f = new Friendship();
        f.setId(id);
        f.setUser(user);
        f.setFriend(friend);
        f.setStatus(status);
        f.setDeleted(false);
        f.setCreatedAt(LocalDateTime.now());
        return f;
    }

    // ---------- Post ----------
    public static Post post(long id, User author, String content, Privacy privacy, VisibilityStatus visibility,
                            ModerationStatus moderation) {
        Post p = new Post();
        p.setId(id);
        p.setUser(author);
        p.setContent(content);
        p.setPrivacy(privacy);
        p.setVisibilityStatus(visibility);
        p.setModerationStatus(moderation);
        p.setNumberOfReacts(java.math.BigInteger.ZERO);
        p.setNumberOfComments(java.math.BigInteger.ZERO);
        p.setNumberOfShares(java.math.BigInteger.ZERO);
        p.setDeleted(false);
        p.setCreatedAt(LocalDateTime.now());
        p.setComments(new ArrayList<>());
        p.setShares(new ArrayList<>());
        return p;
    }

    public static Post post(long id, User author) {
        return post(id, author, "Test content", Privacy.PUBLIC, VisibilityStatus.ACTIVE, ModerationStatus.NONE);
    }

    // ---------- Share ----------
    public static Share share(long id, User user, Post post, String content) {
        Share s = new Share();
        s.setId(id);
        s.setUser(user);
        s.setPost(post);
        s.setContent(content);
        s.setCreatedAt(LocalDateTime.now());
        return s;
    }

    // ---------- Page ----------
    public static org.springframework.data.domain.Pageable pageable() {
        return org.springframework.data.domain.PageRequest.of(0, 10);
    }

    @SuppressWarnings("unchecked")
    public static <T> org.springframework.data.domain.Page<T> emptyPage() {
        return (org.springframework.data.domain.Page<T>) new org.springframework.data.domain.PageImpl<>(
                java.util.Collections.emptyList(), pageable(), 0);
    }

    public static <T> org.springframework.data.domain.Page<T> pageOf(List<T> content) {
        return new org.springframework.data.domain.PageImpl<>(content, pageable(), content.size());
    }
}
