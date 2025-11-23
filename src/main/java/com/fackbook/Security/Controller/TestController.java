package com.fackbook.Security.Controller;

import com.fackbook.Security.RateLimiter.RateLimit;
import com.fackbook.Security.Service.CustomUserDetails;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/test")
@RequiredArgsConstructor
public class TestController {
    @GetMapping("/say-hello")
    @RateLimit(maxRequests = 5, timeWindowMs = 60000)
    public ResponseEntity<?> sayHello(){
        return ResponseEntity.ok("Hello !");
    }
    @PreAuthorize("hasRole('SYSTEM_ADMIN')")
    @GetMapping("/system-admin")
    @RateLimit(maxRequests = 5, timeWindowMs = 60000)
    public ResponseEntity<?> systemAdminAPI(@AuthenticationPrincipal CustomUserDetails principal){
        var userDetails = principal.getUser();
        return ResponseEntity.ok(userDetails.getId()+" "+userDetails.getEmail()+" Hello "+userDetails.getRole().name()+" !");
    }
    @PreAuthorize("hasRole('GROUP_ADMIN')")
    @GetMapping("/group-admin")
    @RateLimit(maxRequests = 5, timeWindowMs = 60000)
    public ResponseEntity<?> groupAdminAPI(@AuthenticationPrincipal CustomUserDetails principal){
        var userDetails = principal.getUser();
        return ResponseEntity.ok(userDetails.getId()+" "+userDetails.getEmail()+" Hello "+userDetails.getRole().name()+" !");
    }
    @PreAuthorize("hasRole('GROUP_MEMBER')")
    @GetMapping("/group-member")
    @RateLimit(maxRequests = 5, timeWindowMs = 60000)
    public ResponseEntity<?> groupMemberAPI(@AuthenticationPrincipal CustomUserDetails principal){
        var userDetails = principal.getUser();
        return ResponseEntity.ok(userDetails.getId()+" "+userDetails.getEmail()+" Hello "+userDetails.getRole().name()+" !");
    }
    @PreAuthorize("hasRole('USER')")
    @GetMapping("/user")
    @RateLimit(maxRequests = 5, timeWindowMs = 60000)
    public ResponseEntity<?> memberAPI(@AuthenticationPrincipal CustomUserDetails principal){
        var userDetails = principal.getUser();
        return ResponseEntity.ok(userDetails.getId()+" "+userDetails.getEmail()+" Hello "+userDetails.getRole().name()+" !");
    }
}
