package com.fackbook.Shared.Helper;

import com.fackbook.Exception.ImageUploadException;
import com.fackbook.Shared.Image.UploadImageService;
import com.fackbook.Shared.Video.YouTubeUploadService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

@Service
@RequiredArgsConstructor
public class FileHelper {
    private final YouTubeUploadService youTubeUploadService;
    private final UploadImageService uploadImageService;
    public  String generateImageUrl(MultipartFile image){
        if(image != null && !image.isEmpty()){
            try {
                return uploadImageService.uploadMultipartFile(image);
            }
            catch (Exception e){
                throw new ImageUploadException("The Image Uploading Process Failed !",e.getCause());
            }
        }
        return null;
    }
    public  String generateVideoUrl(MultipartFile video , String title,String description){
        if(video != null && !video.isEmpty()){
            try {
                return youTubeUploadService.uploadVideo(video,title,description);
            }
            catch (Exception e){
                throw new ImageUploadException("The Video Uploading Process Failed !",e.getCause());
            }
        }
        return null;
    }
}
