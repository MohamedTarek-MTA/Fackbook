package com.fackbook.Shared.Mail.Service;

import com.fackbook.Shared.Mail.DTO.MailDTO;
import lombok.RequiredArgsConstructor;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;

@Service
@RequiredArgsConstructor
public class MailService {
    private final JavaMailSender javaMailSender;

    @Async
    public void sendCodeToViaEmail(MailDTO dto){
        if(!StringUtils.hasText(dto.getEmail()) || !StringUtils.hasText(dto.getVerificationCode())) {
            throw new IllegalArgumentException("Email or verification code is missing!");
        }

        try {
            MimeMessage mimeMessage = javaMailSender.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true, "UTF-8");

            helper.setTo(dto.getEmail());
            helper.setSubject("Verify your Email");

            String htmlContent =
                    """
                    <div style="font-family: Arial, sans-serif; padding: 20px;">
                        <h2 style="color: #1a73e8;">Verification Email</h2>
                        <p style="font-size: 16px;">Your verification code is:</p>
                        <p style="font-size: 28px; font-weight: bold; color: #333;">
                            %s
                        </p>
                        <p>This code will expire shortly.</p>
                    </div>
                    """.formatted(dto.getVerificationCode());

            helper.setText(htmlContent, true);

            javaMailSender.send(mimeMessage);

        } catch (MessagingException e) {
            throw new RuntimeException("Failed to send email", e);
        }
    }
}
