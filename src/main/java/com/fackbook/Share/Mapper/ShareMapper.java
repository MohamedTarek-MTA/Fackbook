package com.fackbook.Share.Mapper;

import com.fackbook.Share.DTO.ShareDTO;
import com.fackbook.Share.Entity.Share;
import org.springframework.stereotype.Component;

@Component
public class ShareMapper {

    public static ShareDTO toDTO(Share share) {
        if (share == null) {
            return null;
        }

        return ShareDTO.builder()
                .id(share.getId())
                .userId(share.getUser() != null ? share.getUser().getId() : null)
                .postId(share.getPost() != null ? share.getPost().getId() : null)
                .content(share.getContent())
                .build();
    }

    public static Share toEntity(ShareDTO shareDTO) {
        if (shareDTO == null) {
            return null;
        }

        return Share.builder()
                .id(shareDTO.getId())
                .content(shareDTO.getContent())
                .build();
    }
}

