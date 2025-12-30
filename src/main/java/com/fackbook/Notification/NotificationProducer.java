package com.fackbook.Notification;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class NotificationProducer {
    private final KafkaTemplate<String,Notification> kafkaTemplate;

    @Value("${notification.topic.name}")
    private String topic;

    public void sendNotification(Notification notification){
        kafkaTemplate.send(
                topic,
                notification.getUserId(),
                notification
        );
    }
}
