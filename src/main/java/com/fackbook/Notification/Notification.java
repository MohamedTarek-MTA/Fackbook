package com.fackbook.Notification;

import lombok.*;
import org.springframework.beans.factory.annotation.Value;

import java.time.Instant;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Notification {
    private String userId;
    private String fromUserId;
    private String targetType;
    private String message;
    @Value("${spring.application.name}")
    private String appName;
    private boolean read = false;
    private Instant createdAt;
}
