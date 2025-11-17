package com.fackbook.Shared.Video.Service;

import com.google.api.services.youtube.YouTube;
import com.google.api.services.youtube.model.Video;
import com.google.api.services.youtube.model.VideoSnippet;
import com.google.api.services.youtube.model.VideoStatus;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;

@Service
@RequiredArgsConstructor
public class YouTubeUploadService {

    private final YouTubeAuthService authService;

    @Value("${youtube.app-name}")
    private String appName;



    public String uploadVideo(MultipartFile file, String title, String description) throws Exception {
        var httpTransport = com.google.api.client.googleapis.javanet.GoogleNetHttpTransport.newTrustedTransport();
        var jsonFactory = com.google.api.client.json.jackson2.JacksonFactory.getDefaultInstance();
        var credential = authService.getCredentials();

        YouTube youtube = new YouTube.Builder(httpTransport, jsonFactory, credential)
                .setApplicationName(appName)
                .build();

        Video videoMeta = new Video();

        VideoStatus status = new VideoStatus();
        status.setPrivacyStatus("unlisted"); // "public" | "unlisted" | "private"
        videoMeta.setStatus(status);

        VideoSnippet snippet = new VideoSnippet();
        snippet.setTitle(title);
        snippet.setDescription(description);
        videoMeta.setSnippet(snippet);

        File tempFile = File.createTempFile("yt-upload-", file.getOriginalFilename());
        file.transferTo(tempFile);

        try (FileInputStream inputStream = new FileInputStream(tempFile)) {
            YouTube.Videos.Insert request = youtube.videos()
                    .insert("snippet,status", videoMeta,
                            new com.google.api.client.http.InputStreamContent("video/*", inputStream));

            Video uploaded = request.execute();
            return "https://www.youtube.com/watch?v=" + uploaded.getId();
        } finally {
            tempFile.delete();
        }
    }
}

