package com.fackbook.Configuration;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.*;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;


@Configuration
public class OpenApiConfig {

    @Bean
    public OpenAPI customOpenAPI() {
        final String bearer = "bearerAuth";
        final String csrf = "XSRF-TOKEN";

        return new OpenAPI()
                .info(new Info()
                        .title("Fackbook API")
                        .version("1.0.0")
                        .description("Backend APIs for Fackbook platform"))
                .addSecurityItem(new SecurityRequirement().addList(bearer))
                .addSecurityItem(new SecurityRequirement().addList(csrf))
                .components(new Components()
                        .addSecuritySchemes(bearer,
                                new SecurityScheme()
                                        .type(SecurityScheme.Type.HTTP)
                                        .scheme("bearer")
                                        .bearerFormat("JWT"))
                        .addSecuritySchemes(csrf,
                                new SecurityScheme()
                                        .type(SecurityScheme.Type.APIKEY)
                                        .in(SecurityScheme.In.HEADER)
                                        .name("X-XSRF-TOKEN")));
    }
}
