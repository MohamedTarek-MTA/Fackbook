package com.fackbook.Shared.Video.Service;

import com.google.api.client.auth.oauth2.Credential;
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow;
import com.google.api.client.googleapis.auth.oauth2.GoogleClientSecrets;
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.client.util.store.FileDataStoreFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.io.FileReader;
import java.util.Collections;

@Service
public class YouTubeAuthService {

    @Value("${youtube.client-secret-path}")
    private String clientSecretPath;

    @Value("${youtube.token-folder}")
    private String tokenFolder;

    public Credential getCredentials() throws Exception {
        var httpTransport = GoogleNetHttpTransport.newTrustedTransport();
        var jsonFactory = JacksonFactory.getDefaultInstance();

        GoogleClientSecrets clientSecrets =
                GoogleClientSecrets.load(jsonFactory, new FileReader(clientSecretPath));

        GoogleAuthorizationCodeFlow flow = new GoogleAuthorizationCodeFlow.Builder(
                httpTransport,
                jsonFactory,
                clientSecrets,
                Collections.singletonList("https://www.googleapis.com/auth/youtube.upload"))
                .setDataStoreFactory(new FileDataStoreFactory(new java.io.File(tokenFolder)))
                .setAccessType("offline")
                .build();

        // Load existing stored credentials (no popup)
        return flow.loadCredential("user");
    }
}
