package com.fackbook.User.Controller;

import com.fackbook.Security.RateLimiter.RateLimit;
import com.fackbook.Security.Service.CustomUserDetails;
import com.fackbook.Shared.Helper.Helper;
import com.fackbook.User.DTO.UserDTO;
import com.fackbook.User.Service.UserService;
import jakarta.validation.constraints.Min;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

@RestController
@RequestMapping("/api/v1/users")
@RequiredArgsConstructor
public class UserController {
    private final UserService userService;
    @GetMapping("/user/{id}")
    @RateLimit
    public ResponseEntity<UserDTO> getUserById(@PathVariable Long id){
        return ResponseEntity.ok(userService.getUserById(id));
    }
    @GetMapping("/email")
    @RateLimit
    public ResponseEntity<UserDTO> getUserByEmail(@RequestParam String email){
        return ResponseEntity.ok(userService.getUserByEmail(email));
    }
    @GetMapping("/phone")
    @RateLimit
    public ResponseEntity<UserDTO> getUserByPhone(@RequestParam String phone){
        return ResponseEntity.ok(userService.getUserByPhone(phone));
    }
    @GetMapping("/male")
    @RateLimit
    public ResponseEntity<Page<UserDTO>> getMaleUsers(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) int size,
            @RequestParam(defaultValue = "name") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ){
        Pageable pageable = Helper.pageHandler(page,size,sortBy,direction);
        return ResponseEntity.ok(userService.getMaleUsers(pageable));
    }
    @GetMapping("/female")
    @RateLimit
    public ResponseEntity<Page<UserDTO>> getFemaleUsers(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) int size,
            @RequestParam(defaultValue = "name") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ){
        Pageable pageable = Helper.pageHandler(page,size,sortBy,direction);
        return ResponseEntity.ok(userService.getMaleUsers(pageable));
    }
    @GetMapping("/system-admin")
    @RateLimit
    @PreAuthorize("hasRole('SYSTEM_ADMIN')")
    public ResponseEntity<Page<UserDTO>> getSystemAdmins(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) int size,
            @RequestParam(defaultValue = "name") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ){
        Pageable pageable = Helper.pageHandler(page,size,sortBy,direction);
        return ResponseEntity.ok(userService.getSystemAdminUsers(pageable));
    }
    @GetMapping("/group-admin")
    @RateLimit
    @PreAuthorize("hasRole('SYSTEM_ADMIN')")
    public ResponseEntity<Page<UserDTO>> getGroupAdmins(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) int size,
            @RequestParam(defaultValue = "name") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ){
        Pageable pageable = Helper.pageHandler(page,size,sortBy,direction);
        return ResponseEntity.ok(userService.getGroupAdminUsers(pageable));
    }
    @GetMapping("/group-member")
    @RateLimit
    @PreAuthorize("hasAnyRole('SYSTEM_ADMIN','GROUP_ADMIN')")
    public ResponseEntity<Page<UserDTO>> getGroupMembers(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) int size,
            @RequestParam(defaultValue = "name") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ){
        Pageable pageable = Helper.pageHandler(page,size,sortBy,direction);
        return ResponseEntity.ok(userService.getGroupMemberUsers(pageable));
    }
    @GetMapping("/normal-user")
    @RateLimit
    @PreAuthorize("hasRole('SYSTEM_ADMIN')")
    public ResponseEntity<Page<UserDTO>> getNormalUsers(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) int size,
            @RequestParam(defaultValue = "name") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ){
        Pageable pageable = Helper.pageHandler(page,size,sortBy,direction);
        return ResponseEntity.ok(userService.getNormalUsers(pageable));
    }
    @GetMapping("/")
    @RateLimit
    @PreAuthorize("hasRole('SYSTEM_ADMIN')")
    public ResponseEntity<Page<UserDTO>> getAllUsers(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) int size,
            @RequestParam(defaultValue = "name") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ){
        Pageable pageable = Helper.pageHandler(page,size,sortBy,direction);
        return ResponseEntity.ok(userService.getAllUsers(pageable));
    }
    @GetMapping("/active")
    @RateLimit
    @PreAuthorize("hasRole('SYSTEM_ADMIN')")
    public ResponseEntity<Page<UserDTO>> getActiveUsers(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) int size,
            @RequestParam(defaultValue = "name") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ){
        Pageable pageable = Helper.pageHandler(page,size,sortBy,direction);
        return ResponseEntity.ok(userService.getActiveUsers(pageable));
    }
    @GetMapping("/inactive")
    @RateLimit
    @PreAuthorize("hasRole('SYSTEM_ADMIN')")
    public ResponseEntity<Page<UserDTO>> getInactiveUsers(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) int size,
            @RequestParam(defaultValue = "name") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ){
        Pageable pageable = Helper.pageHandler(page,size,sortBy,direction);
        return ResponseEntity.ok(userService.getInactiveUsers(pageable));
    }
    @GetMapping("/deleted")
    @RateLimit
    @PreAuthorize("hasRole('SYSTEM_ADMIN')")
    public ResponseEntity<Page<UserDTO>> getDeletedUsers(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) int size,
            @RequestParam(defaultValue = "name") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ){
        Pageable pageable = Helper.pageHandler(page,size,sortBy,direction);
        return ResponseEntity.ok(userService.getDeletedUsers(pageable));
    }
    @GetMapping("/banned")
    @RateLimit
    @PreAuthorize("hasRole('SYSTEM_ADMIN')")
    public ResponseEntity<Page<UserDTO>> getBannedUsers(
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) int size,
            @RequestParam(defaultValue = "name") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ){
        Pageable pageable = Helper.pageHandler(page,size,sortBy,direction);
        return ResponseEntity.ok(userService.getBannedUsers(pageable));
    }
    @GetMapping("/name")
    @RateLimit
    public ResponseEntity<Page<UserDTO>> getUsersByName(
            @RequestParam String name,
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) int size,
            @RequestParam(defaultValue = "name") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ){
        Pageable pageable = Helper.pageHandler(page,size,sortBy,direction);
        return ResponseEntity.ok(userService.getUsersByName(name,pageable));
    }
    @GetMapping("/address")
    @RateLimit
    public ResponseEntity<Page<UserDTO>> getUsersByAddress(
            @RequestParam String address,
            @RequestParam(defaultValue = "0") @Min(0) int page,
            @RequestParam(defaultValue = "10") @Min(1) int size,
            @RequestParam(defaultValue = "name") String sortBy,
            @RequestParam(defaultValue = "asc") String direction
    ){
        Pageable pageable = Helper.pageHandler(page,size,sortBy,direction);
        return ResponseEntity.ok(userService.getUsersByAddress(address,pageable));
    }
    @PatchMapping("/to-system-admin/{id}")
    @RateLimit
    @PreAuthorize("hasRole('SYSTEM_ADMIN')")
    public ResponseEntity<UserDTO> userToSystemAdmin(@PathVariable Long id){
        return ResponseEntity.ok(userService.toSystemAdmin(id));
    }
    @PatchMapping("/to-group-admin/{id}")
    @RateLimit
    @PreAuthorize("hasAnyRole('SYSTEM_ADMIN')")
    public ResponseEntity<UserDTO> userToGroupAdmin(@PathVariable Long id){
        return ResponseEntity.ok(userService.toGroupAdmin(id));
    }
    @PatchMapping("/to-group-member/{id}")
    @RateLimit
    @PreAuthorize("hasAnyRole('SYSTEM_ADMIN')")
    public ResponseEntity<UserDTO> userToGroupMember(@PathVariable Long id){
        return ResponseEntity.ok(userService.toGroupMember(id));
    }
    @PatchMapping("/to-normal-user/{id}")
    @RateLimit
    @PreAuthorize("hasAnyRole('SYSTEM_ADMIN')")
    public ResponseEntity<UserDTO> userToNormalUser(@PathVariable Long id){
        return ResponseEntity.ok(userService.toNormalUser(id));
    }
    @PatchMapping("/active-user/{id}")
    @RateLimit
    @PreAuthorize("hasAnyRole('SYSTEM_ADMIN')")
    public ResponseEntity<UserDTO> activeUser(@PathVariable Long id){
        return ResponseEntity.ok(userService.activeUser(id));
    }
    @PatchMapping("/inactive-user/{id}")
    @RateLimit
    @PreAuthorize("hasAnyRole('SYSTEM_ADMIN')")
    public ResponseEntity<UserDTO> inactiveUser(@PathVariable Long id){
        return ResponseEntity.ok(userService.inactiveUser(id));
    }
    @PatchMapping("/ban-user/{id}")
    @RateLimit
    @PreAuthorize("hasAnyRole('SYSTEM_ADMIN')")
    public ResponseEntity<UserDTO> banUser(@PathVariable Long id){
        return ResponseEntity.ok(userService.banUser(id));
    }
    @DeleteMapping("/delete-user/{id}")
    @RateLimit
    @PreAuthorize("hasAnyRole('SYSTEM_ADMIN')")
    public ResponseEntity<UserDTO> deleteUser(@PathVariable Long id){
        return ResponseEntity.ok(userService.deleteUser(id));
    }
    @PutMapping("/update-user")
    @RateLimit
    public ResponseEntity<UserDTO> updateUser(@AuthenticationPrincipal CustomUserDetails userDetails,@RequestPart(required = false) UserDTO dto,@RequestPart(required = false)MultipartFile image){
        return ResponseEntity.ok(userService.updateUser(userDetails.getId(),dto,image));
    }
}
