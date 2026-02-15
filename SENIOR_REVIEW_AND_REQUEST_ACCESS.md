# Senior Software Engineer Review – Fackbook

## Part 1: Weaknesses and Enhancements

---

### Logic

| Issue | Severity | Description | Enhancement |
|-------|----------|-------------|-------------|
| **setRequestAsRejected calls handleGroupMembership** | **High** | In `RequestService.setRequestAsRejected`, for `GROUP_JOIN_REQUEST` the code calls `groupMemberService.handleGroupMembership(...)`. Rejecting a request should **not** add the user to the group. This is a copy-paste error from `setRequestAsAccepted`. | Remove the `handleGroupMembership` block from `setRequestAsRejected` for `GROUP_JOIN_REQUEST` (and do not add the user on reject). |
| **CONTENT_APPROVAL targetId** | Medium | PostService/CommentService create requests with `targetId = groupId` for CONTENT_APPROVAL, but `resolveTargetId` in RequestService expects `targetId = postId` (it does `postRepository.findById(targetId)`). This will throw "Post Not Found" when creating a post/comment that requires approval. | Create the post/comment first (with PENDING_APPROVAL), then create the request with `targetId = post.getId()` or `comment.getId()`, and in `resolveTargetId` for CONTENT_APPROVAL resolve by `targetType` (POST → postRepository, COMMENT → commentRepository). |
| **GROUP_INVITE semantics** | Medium | For GROUP_INVITE, it is unclear who is "sender" vs "receiver": `user` and `targetId` (groupId) do not model "admin invites user X" cleanly (who is X?). | Add an explicit field (e.g. `recipientUserId`) for invite target, or document a single convention (e.g. `user` = invited person, and the invite is created by group admin) and enforce it everywhere. |
| **Duplicate request check** | Low | A user could create multiple PENDING GROUP_JOIN_REQUESTs for the same group. | Before creating a request, check `findByUser_IdAndTargetIdAndActionType(userId, targetId, actionType)` and if a PENDING one exists, throw or return existing. |
| **setRequestAsPending logic** | Low | `setRequestAsPending` checks `if (!request.getStatus().equals(Status.PENDING))` and then throws – so it only "succeeds" when already PENDING, which is a no-op. | Either remove this method or define the intended behavior (e.g. "reopen" a rejected/canceled request). |

---

### Security

| Issue | Severity | Description | Enhancement |
|-------|----------|-------------|-------------|
| **No authorization on request operations** | **High** | RequestService has no notion of "current user". Any caller (e.g. a controller) that passes a `requestId` can get/accept/reject any request. There is no check that the caller is the sender, receiver, system admin, or group admin. | Enforce access in service or controller: only allow get/accept/reject if the current user is allowed (see Part 2 – Request access control). |
| **UserController exposes all users by criteria** | Medium | Endpoints like `getUserById`, `getUserByEmail`, `getUserByPhone`, `getMaleUsers`, etc. are not restricted by role. Any authenticated user can list users by gender/role/status. | Restrict listing endpoints to SYSTEM_ADMIN (you already do for some); ensure get-by-id/email/phone are either public (profile) or restricted (e.g. self or admin only). |
| **CSRF disabled** | Medium | `SecurityConfig` has `csrf(AbstractHttpConfigurer::disable)`. For a stateless JWT API this is common, but if you ever add cookie-based auth or browser forms, re-enable CSRF. | Document that the API is JWT-only and not browser-form-based; if you add cookies/sessions, enable CSRF. |
| **Sensitive data in responses** | Low | Ensure UserDTO and other DTOs do not expose password, verificationCode, or internal IDs that could aid enumeration. | Audit DTOs; exclude sensitive fields from serialization. |
| **No rate limiting on sensitive operations** | Low | Only some endpoints use `@RateLimit`. Create account, login, password reset, and request create/accept/reject could be rate-limited. | Apply rate limiting to auth and request mutation endpoints. |

---

### Performance

| Issue | Severity | Description | Enhancement |
|-------|----------|-------------|-------------|
| **N+1 on list endpoints** | Medium | Services often load an entity then map to DTO; if the entity has lazy collections (e.g. Group.members, User.friendships), iterating in mappers can cause N+1 queries. | Use `@EntityGraph` or `JOIN FETCH` in repositories for the associations needed when mapping to DTOs; or use a DTO projection query. |
| **Global list methods without user scope** | Medium | Methods like `getGroupJoinRequests(pageable)` return **all** requests of that type in the system. At scale this is expensive and usually not what you want. | Prefer user-scoped or resource-scoped APIs (e.g. "my sent requests", "requests for my group", "requests I received") and paginate. |
| **Cache key collisions** | Low | Cache keys like `userId + '-' + groupId` can collide if IDs are large or concatenation is ambiguous. | Use a dedicated key generator (e.g. `"user:" + userId + ":group:" + groupId`) and consider caching only where it clearly wins (e.g. hot paths). |
| **No pagination limits** | Low | Controllers accept `page` and `size` from the client; a very large `size` could stress the DB. | Cap `size` (e.g. max 100) in controller or Pageable config. |

---

### Other Enhancements

1. **Consistent exception types**  
   Replace generic `IllegalArgumentException` with domain exceptions (e.g. `ResourceNotFoundException`, `AccessDeniedException`) and map them in `GlobalExceptionHandler` to appropriate HTTP status codes (404, 403).

2. **Idempotency for accept/reject**  
   Consider idempotency keys for accept/reject so duplicate client requests do not change state twice.

3. **Audit fields**  
   Add `acceptedByUserId` / `rejectedByUserId` and timestamps on Request for auditing who acted on a request and when.

4. **Validation**  
   Validate that `targetType` and `actionType` match (e.g. GROUP_JOIN_REQUEST implies targetType GROUP) in RequestService or via DTO validation.

---

## Part 2: Request Access Control (Your Requirement)

You want request visibility and actions to be restricted so that:

- **System admin** can access (view/accept/reject) any request.
- **Sender** of the request can access it (view, cancel).
- **Receiver** of the request can access it (view, accept/reject).
- **Group admin** can access requests that target **their own group** (e.g. GROUP_JOIN_REQUEST / GROUP_INVITE where `targetId` = their group).

Below is the **semantic model** and the **implementation** added to your project.

### Who is "receiver" per request type?

| Action Type        | Sender           | Receiver / Who can act |
|--------------------|------------------|-------------------------|
| FRIENDSHIP_REQUEST | `request.getUser()` | User with id = `targetId` (the friend) |
| GROUP_JOIN_REQUEST | `request.getUser()` | Group owner (`group.getUser()`) or any admin of that group |
| GROUP_INVITE       | Group owner (inviter) | Invited user = `request.getUser()` (if you use "user = invited" convention) |
| CONTENT_APPROVAL   | `request.getUser()` | Owner of the post/comment (resolve from `targetId` + `targetType`) |

### Access rules (who can **see** and **act on** a request)

- **System admin** (role SYSTEM_ADMIN): can see and act on any request.
- **Sender**: `request.getUser().getId().equals(currentUserId)` → can see, cancel.
- **Receiver**: as in the table above (e.g. for GROUP_JOIN_REQUEST, receiver = group owner or group admin of that group).
- **Group admin**: for GROUP_JOIN_REQUEST and GROUP_INVITE, if `request.getTargetId()` is a group id and the current user is the **owner** of that group or a **GROUP_ADMIN** member of that group → can see and accept/reject.

Implementation in your codebase:

1. **RequestAccessService** (new) – encapsulates "can current user access this request?" and "requests visible to user".
2. **RequestService** – new methods:
   - `getRequestByIdIfCanAccess(Long requestId, Long currentUserId)` – returns request only if the user is allowed; otherwise throws.
   - `getRequestsVisibleToUser(Long currentUserId, Pageable pageable)` – returns a single page of requests that the user is allowed to see (sender, receiver, system admin, or group admin for their groups).
3. **RequestRepository** – new methods used by visibility:
   - e.g. `findByUser_IdAndActionType`, `findByTargetIdAndActionType` (you have the latter), and for "group join requests for my groups" a method that takes a list of group ids.
4. **Bug fix** – remove the `handleGroupMembership` call from `setRequestAsRejected` for GROUP_JOIN_REQUEST.

### What was implemented

1. **Bug fix** – Removed the incorrect `handleGroupMembership` call from `setRequestAsRejected` (reject no longer adds the user to the group).
2. **RequestRepository** – Added `findByUser_IdAndActionType`, `findByTargetIdInAndActionType`, and `findVisibleToUser(userId, groupIds, pageable)` for visibility filtering.
3. **GroupMemberRepository** – Added `findByUser_IdAndRole` to resolve groups where the user is GROUP_ADMIN.
4. **RequestService** – New methods:
   - `getRequestByIdIfCanAccess(requestId, currentUserId)` – returns request DTO only if the user is allowed; otherwise throws.
   - `getRequestsVisibleToUser(currentUserId, pageable)` – returns a page of requests visible to the user (system admin sees all; others see sent by me, received friend, group join/invite for their groups).
   - `canAccessRequest(request, currentUserId)` – returns whether the user can view/access the request.
   - `canAcceptOrRejectRequest(request, currentUserId)` – true if user is receiver or system admin.
   - `canCancelRequest(request, currentUserId)` – true if user is sender or system admin.
   - `setRequestAsAcceptedIfCanAccess(requestId, currentUserId)` – accept only if allowed.
   - `setRequestAsRejectedIfCanAccess(requestId, currentUserId)` – reject only if allowed.
   - `setRequestAsCanceledIfCanAccess(requestId, currentUserId)` – cancel only if allowed.

**CONTENT_APPROVAL** receiver is not yet implemented in `canAccessRequest` (returns false); you can add resolution of post/comment owner later if needed.

### How to use from a RequestController

- Resolve current user id from the principal, e.g. `@AuthenticationPrincipal CustomUserDetails principal` then `principal.getId()`.
- **List requests (visible only):**  
  `GET /api/v1/requests/visible?page=0&size=10`  
  → call `requestService.getRequestsVisibleToUser(principal.getId(), pageable)`.
- **Get one request (if allowed):**  
  `GET /api/v1/requests/{id}`  
  → call `requestService.getRequestByIdIfCanAccess(id, principal.getId())`; if it throws, return 403.
- **Accept / Reject / Cancel:**  
  Use `setRequestAsAcceptedIfCanAccess`, `setRequestAsRejectedIfCanAccess`, `setRequestAsCanceledIfCanAccess` with `requestId` and `principal.getId()`; on exception return 403.

---

## Summary

- **Logic**: Fix reject (do not call handleGroupMembership on reject); fix CONTENT_APPROVAL targetId and optionally GROUP_INVITE semantics; add duplicate-request check.
- **Security**: Enforce request access control (system admin, sender, receiver, group admin); tighten user listing; document CSRF; avoid leaking sensitive data; rate-limit sensitive ops.
- **Performance**: Avoid N+1, prefer user/resource-scoped listing, safe cache keys, cap page size.

Request visibility and actions are now restricted so that only system admin, sender, receiver, or group admin (for their group) can access the relevant requests, as implemented in the new and updated code.
