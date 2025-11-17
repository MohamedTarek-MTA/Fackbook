package com.fackbook.Configuration;

import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.HttpClientBuilder;
import org.apache.hc.core5.util.Timeout;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestTemplate;

@Configuration
public class RestTemplateConfig {

    @Bean
    public RestTemplate restTemplate() {
        CloseableHttpClient client = HttpClientBuilder.create()
                .setDefaultRequestConfig(
                        org.apache.hc.client5.http.config.RequestConfig.custom()
                                .setConnectTimeout(Timeout.ofSeconds(10))
                                .setResponseTimeout(Timeout.ofSeconds(20))
                                .build()
                )
                .build();

        var factory = new HttpComponentsClientHttpRequestFactory(client);
        return new RestTemplate(factory);
    }
}
