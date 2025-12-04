package com.fackbook.Post.Util.Service;

import com.fackbook.Post.Util.Interface.MediaAttachable;
import com.fackbook.Shared.Helper.FileHelper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

@Service
@RequiredArgsConstructor
public class MediaManager {
    private final FileHelper fileHelper;

    public <T extends MediaAttachable> void handleMedia(
            T entity,
            MultipartFile newImage,
            MultipartFile newVideo,
            Boolean removeImage,
            Boolean removeVideo
    ) {

        // Prevent remove + upload conflict
        if (newImage != null && Boolean.TRUE.equals(removeImage))
            throw new IllegalArgumentException("Cannot upload & remove image at same time");

        if (newVideo != null && Boolean.TRUE.equals(removeVideo))
            throw new IllegalArgumentException("Cannot upload & remove video at same time");

        // --- Image handling
        if (Boolean.TRUE.equals(removeImage)) entity.setImageUrl(null);
        else if (newImage != null) entity.setImageUrl(fileHelper.generateImageUrl(newImage));

        // --- Video handling
        if (Boolean.TRUE.equals(removeVideo)) entity.setVideoUrl(null);
        else if (newVideo != null) entity.setVideoUrl(
                fileHelper.generateVideoUrl(newVideo, "Updated Entity", "Media Update")
        );


    }
}

