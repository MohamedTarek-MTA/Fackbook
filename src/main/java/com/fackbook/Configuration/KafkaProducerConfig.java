package com.fackbook.Configuration;

import com.fackbook.Notification.Notification;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.core.ProducerFactory;

@Configuration
public class KafkaProducerConfig {
    @Bean
    KafkaTemplate<String, Notification> kafkaTemplate(
            ProducerFactory<String,Notification> producerFactory
    ){
        return new KafkaTemplate<>(producerFactory);
    }
}
