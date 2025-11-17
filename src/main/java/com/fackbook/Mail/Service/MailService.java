package com.fackbook.Mail.Service;

import com.fackbook.Mail.DTO.MailDTO;
import lombok.RequiredArgsConstructor;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

@Service
@RequiredArgsConstructor
public class MailService {
    private final JavaMailSender javaMailSender;
    @Async
    public void sendCodeToViaEmail(MailDTO dto){
        if(!StringUtils.hasText(dto.getEmail()) || !StringUtils.hasText(dto.getVerificationCode())) {
            throw new IllegalArgumentException("Email or verification code is missing!");
        }
        SimpleMailMessage message = new SimpleMailMessage();
        message.setTo(dto.getEmail());
        message.setSubject("Verify your email");
        message.setText("Your verification code is: " + dto.getVerificationCode());
        javaMailSender.send(message);
    }

}
