package com.fackbook.Security.Controller;

import com.fackbook.Security.DTO.AuthRequest;
import com.fackbook.Security.DTO.RegisterRequest;
import com.fackbook.Security.DTO.ResendCodeRequest;
import com.fackbook.Security.DTO.ResetPasswordRequest;
import com.fackbook.Security.RateLimiter.RateLimit;
import com.fackbook.Security.Service.AuthService;
import com.fackbook.Shared.Mail.DTO.MailDTO;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/auth")
@RequiredArgsConstructor
public class AuthController {
    private final AuthService authService;
    @PostMapping("/register")
    @RateLimit(maxRequests = 5, timeWindowMs = 30000)
    public ResponseEntity<?> register(@Valid @RequestBody RegisterRequest request){
        return ResponseEntity.ok(authService.register(request));
    }
    @PostMapping("/verify-account")
    @RateLimit(maxRequests = 1, timeWindowMs = 1500)
    public ResponseEntity<?> verifyAccount(@Valid @RequestBody MailDTO dto){
        return ResponseEntity.ok(authService.verifyAccount(dto));
    }
    @PostMapping("/login")
    @RateLimit(maxRequests = 5, timeWindowMs = 30000)
    public ResponseEntity<?> login(@Valid @RequestBody AuthRequest request){
        return ResponseEntity.ok(authService.login(request));
    }
    @PostMapping("/reset-password")
    @RateLimit(maxRequests = 5, timeWindowMs = 30000)
    public ResponseEntity<?> resetPassword(@Valid @RequestBody ResetPasswordRequest request){
        return ResponseEntity.ok(authService.resetPassword(request));
    }
    @PostMapping("/resend-code")
    @RateLimit(maxRequests = 1, timeWindowMs = 1500)
    public ResponseEntity<?> resendCode(@Valid @RequestBody ResendCodeRequest request){
        return ResponseEntity.ok(authService.resendVerificationCode(request));
    }
}
