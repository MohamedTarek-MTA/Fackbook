package com.fackbook.Security.Service;

import com.fackbook.Security.DTO.*;
import com.fackbook.Security.Util.JwtUtil;
import com.fackbook.Shared.Helper.Helper;
import com.fackbook.Shared.Mail.DTO.MailDTO;
import com.fackbook.Shared.Mail.Mapper.MailMapper;
import com.fackbook.Shared.Mail.Service.MailService;
import com.fackbook.User.Entity.User;
import com.fackbook.User.Enum.Status;
import com.fackbook.User.Repository.UserRepository;
import com.fackbook.User.Service.UserService;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;


@Service
@RequiredArgsConstructor
@Slf4j
public class AuthService {
    private final AuthenticationManager authenticationManager;
    private final JwtUtil jwtUtil;
    private final PasswordEncoder passwordEncoder;
    private final UserService userService;
    private final UserRepository userRepository;
    private final MailService mailService;

    @Transactional
    public String register(RegisterRequest request){
        if(userService.userExistsByEmail(request.getEmail())){
            throw new IllegalArgumentException("This Email Already Exists !");
        }
        if(userService.userExistsByPhone(request.getPhone())){
            throw new IllegalArgumentException("This Phone Number Already Exists !");
        }
        try{
            String verificationCode = Helper.generateCode();
            User user = User.builder()
                    .name(request.getFirstName()+" "+request.getLastName())
                    .email(request.getEmail())
                    .password(passwordEncoder.encode(request.getPassword()))
                    .phone(request.getPhone())
                    .address(request.getAddress())
                    .birthdate(request.getBirthdate())
                    .gender(request.getGender())
                    .role(request.getRole())
                    .bio(null)
                    .lastLoginDate(null)
                    .createdAt(LocalDateTime.now())
                    .status(Status.INACTIVE)
                    .enabled(false)
                    .deleted(false)
                    .deletedAt(null)
                    .updatedAt(null)
                    .verificationCode(verificationCode)
                    .build();
            userRepository.save(user);
            mailService.sendCodeToViaEmail(MailMapper.toDTO(user.getEmail(),verificationCode));
            return "Please Check Your Email to Get Verification Code !!";
        }catch (Exception e){
            log.error("Error occurred while registering user", e);
            throw new RuntimeException("An unexpected error occurred. Please try again later.");
        }
    }
    public AuthResponse login(AuthRequest request){
        authenticationManager.authenticate(
                new UsernamePasswordAuthenticationToken(request.getEmail(),request.getPassword())
        );
        var user = userService.getUserEntityByEmail(request.getEmail());
        if(user.getDeleted()){
            throw new IllegalArgumentException("This Account Has Been Deleted !");
        }
        if(!user.getStatus().equals(Status.ACTIVE)){
            throw new IllegalArgumentException("Your account is currently " + user.getStatus().name().toLowerCase() + ". Please verify or contact support.");
        }
        String token = jwtUtil.generateToken(user.getId(),user.getEmail(),user.getRole().name(),user.getStatus().name());
        userService.updateLastLoginDateById(user.getId());
        return new AuthResponse(token);
    }

    public String verifyAccount(MailDTO dto){
        var user = userService.getUserEntityByEmail(dto.getEmail());
        if(user.getEnabled() || user.getVerificationCode() == null){
            throw new IllegalArgumentException("This account already verified !");
        }
        if(!user.getVerificationCode().equals(dto.getVerificationCode())){
            throw new IllegalArgumentException("Verification Code Didn't Match !!");
        }
        user.setVerificationCode(null);
        userService.activeUser(user.getId());
        return "Your Account Has Been Verified Successfully !";
    }

    public String resendVerificationCode(ResendCodeRequest request){
        var user = userService.getUserEntityByEmail(request.getEmail());
        if(user.getVerificationCode() == null || user.getEnabled()){
            throw new IllegalArgumentException("This account is already verified !");
        }
        try{
            String verificationCode = Helper.generateCode();
            user.setVerificationCode(verificationCode);
            userRepository.save(user);
            mailService.sendCodeToViaEmail(MailMapper.toDTO(user.getEmail(),verificationCode));
            return "Please Check Your Email to Get Verification Code !!";
        }catch (Exception e){
            throw new RuntimeException("Something Wrong happened please try again ",e.getCause());
        }
    }

    public String resetPassword(ResetPasswordRequest request){
        var user = userService.getUserEntityByEmail(request.getEmail());

        if(user.getDeleted()){
            throw new IllegalArgumentException("This Account Has Been Deleted !");
        }
        if(!user.getStatus().equals(Status.ACTIVE)){
            throw new IllegalArgumentException("You Couldn't Change Your Current Password Because Your Account Is "+user.getStatus().name());
        }
        if(!request.getPhone().equals(user.getPhone())){
            throw new IllegalArgumentException("Invalid Phone Number !");
        }
        if(!request.getNewPassword().equals(request.getConfirmedNewPassword())){
            throw new IllegalArgumentException("Passwords Don't Match !");
        }
        user.setPassword(passwordEncoder.encode(request.getNewPassword()));
        userRepository.save(user);
        return "Password Changed Successfully !";
    }
}
