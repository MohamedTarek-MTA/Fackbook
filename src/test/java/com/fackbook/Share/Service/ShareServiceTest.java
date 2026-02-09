package com.fackbook.Share.Service;

import com.fackbook.Post.Entity.Post;
import com.fackbook.Post.Service.PostService;
import com.fackbook.Share.DTO.ShareDTO;
import com.fackbook.Share.Entity.Share;
import com.fackbook.Share.Repository.ShareRepository;
import com.fackbook.TestFixtures;
import com.fackbook.User.Entity.User;
import com.fackbook.User.Service.UserService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("ShareService tests")
class ShareServiceTest {

    @Mock
    private ShareRepository shareRepository;

    @Mock
    private PostService postService;

    @Mock
    private UserService userService;

    @InjectMocks
    private ShareService shareService;

    private User user;
    private Post post;
    private Share share;
    private ShareDTO shareDTO;
    private Pageable pageable;

    @BeforeEach
    void setUp() {
        user = TestFixtures.user(1L);
        post = TestFixtures.post(10L, user);
        share = TestFixtures.share(1L, user, post, "Shared!");
        shareDTO = ShareDTO.builder().content("Shared!").build();
        pageable = TestFixtures.pageable();
    }

    @Nested
    @DisplayName("getShareById")
    class GetShareById {
        @Test
        void returnsDTOWhenFound() {
            when(shareRepository.findById(1L)).thenReturn(Optional.of(share));
            var result = shareService.getShareById(1L);
            assertThat(result).isNotNull();
        }

        @Test
        void throwsWhenNotFound() {
            when(shareRepository.findById(999L)).thenReturn(Optional.empty());
            assertThatThrownBy(() -> shareService.getShareById(999L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("not shared");
        }
    }

    @Nested
    @DisplayName("getSharesByUserId / getSharesByPostId")
    class GetShares {
        @Test
        void getSharesByUserIdReturnsPage() {
            when(shareRepository.findByUser_Id(1L, pageable)).thenReturn(new PageImpl<>(List.of(share)));
            Page<?> result = shareService.getSharesByUserId(1L, pageable);
            assertThat(result).isNotEmpty();
        }

        @Test
        void getSharesByPostIdReturnsPage() {
            when(shareRepository.findByPost_Id(10L, pageable)).thenReturn(new PageImpl<>(List.of(share)));
            Page<?> result = shareService.getSharesByPostId(10L, pageable);
            assertThat(result).isNotEmpty();
        }
    }

    @Nested
    @DisplayName("createNewShare")
    class CreateNewShare {
        @Test
        void createsShareAndUpdatesPostCount() {
            when(userService.getUserEntityById(1L)).thenReturn(user);
            when(postService.getPostEntityById(10L)).thenReturn(post);
            when(shareRepository.save(any(Share.class))).thenAnswer(i -> {
                Share s = i.getArgument(0);
                s.setId(1L);
                return s;
            });
            doNothing().when(postService).savePost(any(Post.class));

            var result = shareService.createNewShare(1L, 10L, shareDTO);

            assertThat(result).isNotNull();
            verify(shareRepository).save(any(Share.class));
            verify(postService).savePost(post);
        }
    }

    @Nested
    @DisplayName("getAllShares")
    class GetAllShares {
        @Test
        void returnsPageOfShares() {
            when(shareRepository.findAll(pageable)).thenReturn(new PageImpl<>(List.of(share)));
            Page<?> result = shareService.getAllShares(pageable);
            assertThat(result).isNotEmpty();
        }
    }
}
