package com.fackbook.Mail.Mapper;

import com.fackbook.Mail.DTO.MailDTO;
import org.springframework.stereotype.Component;

@Component
public class MailMapper {
    public static MailDTO toDTO(String email, String verificationCode) {
        if (email == null || email.trim().isEmpty() ||
                verificationCode == null || verificationCode.trim().isEmpty()) {
            return null;
        }

        return MailDTO.builder()
                .email(email.trim())
                .verificationCode(verificationCode.trim())
                .build();
    }

}
