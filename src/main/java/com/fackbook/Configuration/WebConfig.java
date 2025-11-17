package com.fackbook.Configuration;

import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
@RequiredArgsConstructor
public class WebConfig implements WebMvcConfigurer{
    @Value("${app.cors.allowed-origins}")
    private String[] allowedOrigins;
    @Override
    public  void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/**")
                .allowedOrigins(allowedOrigins) // React dev server
                .allowedMethods("*")
                .allowedHeaders("*")
                .allowCredentials(true);
    }

}