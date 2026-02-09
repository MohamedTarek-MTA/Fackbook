package com.fackbook.Report.Service;

import com.fackbook.Report.Entity.Report;
import com.fackbook.Report.Repository.ReportRepository;
import com.fackbook.Request.Enum.RequestTargetType;
import com.fackbook.Request.Enum.Status;
import com.fackbook.TestFixtures;
import com.fackbook.User.Entity.User;
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
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("ReportService tests")
class ReportServiceTest {

    @Mock
    private ReportRepository reportRepository;

    @Mock
    private com.fackbook.User.Repository.UserRepository userRepository;

    @Mock
    private com.fackbook.Group.Repository.GroupRepository groupRepository;

    @Mock
    private com.fackbook.Group.Repository.GroupMemberRepository groupMemberRepository;

    @Mock
    private com.fackbook.Post.Repository.PostRepository postRepository;

    @Mock
    private com.fackbook.Comment.Repository.CommentRepository commentRepository;

    @Mock
    private com.fackbook.Reply.Repository.ReplyRepository replyRepository;

    @Mock
    private com.fackbook.Shared.Helper.FileHelper fileHelper;

    @InjectMocks
    private ReportService serviceUnderTest;

    private User user;
    private Report report;
    private Pageable pageable;

    @BeforeEach
    void setUp() {
        user = TestFixtures.user(1L);
        report = Report.builder()
                .id(1L)
                .user(user)
                .targetId(10L)
                .targetType(RequestTargetType.POST)
                .status(Status.PENDING)
                .content("Report content")
                .build();
        pageable = TestFixtures.pageable();
    }

    @Nested
    @DisplayName("getReportEntityById / getReport")
    class GetReport {
        @Test
        void getReportEntityByIdReturnsReportWhenFound() {
            when(reportRepository.findById(1L)).thenReturn(Optional.of(report));
            Report result = serviceUnderTest.getReportEntityById(1L);
            assertThat(result).isSameAs(report);
        }

        @Test
        void getReportEntityByIdThrowsWhenNotFound() {
            when(reportRepository.findById(999L)).thenReturn(Optional.empty());
            assertThatThrownBy(() -> serviceUnderTest.getReportEntityById(999L))
                    .isInstanceOf(IllegalArgumentException.class)
                    .hasMessageContaining("Report Not Found");
        }

        @Test
        void getReportReturnsDTOWhenFound() {
            when(reportRepository.findById(1L)).thenReturn(Optional.of(report));
            var result = serviceUnderTest.getReport(1L);
            assertThat(result).isNotNull();
        }
    }

    @Nested
    @DisplayName("getReportsByUserId")
    class GetReportsByUserId {
        @Test
        void returnsPageOfReports() {
            when(reportRepository.findByUser_Id(1L, pageable)).thenReturn(new PageImpl<>(List.of(report)));
            Page<?> result = serviceUnderTest.getReportsByUserId(1L, pageable);
            assertThat(result).isNotEmpty();
        }
    }

    @Nested
    @DisplayName("getReportsOnPosts / getReportsOnUsers")
    class GetReportsOnTarget {
        @Test
        void getReportsOnPostsReturnsPage() {
            when(reportRepository.findByTargetIdAndTargetType(10L, RequestTargetType.POST, pageable))
                    .thenReturn(new PageImpl<>(List.of(report)));
            Page<?> result = serviceUnderTest.getReportsOnPosts(10L, pageable);
            assertThat(result).isNotEmpty();
        }

        @Test
        void getReportsOnUsersReturnsPage() {
            when(reportRepository.findByTargetIdAndTargetType(1L, RequestTargetType.USER, pageable))
                    .thenReturn(new PageImpl<>(List.of()));
            Page<?> result = serviceUnderTest.getReportsOnUsers(1L, pageable);
            assertThat(result).isEmpty();
        }
    }

    @Nested
    @DisplayName("setReportAsAccepted / setReportAsRejected")
    class ChangeReportStatus {
        @Test
        void setReportAsAcceptedUpdatesStatusAndSaves() {
            when(reportRepository.findById(1L)).thenReturn(Optional.of(report));
            when(userRepository.findById(1L)).thenReturn(Optional.of(user));
            when(reportRepository.save(any(Report.class))).thenAnswer(i -> i.getArgument(0));

            var result = serviceUnderTest.setReportAsAccepted(1L, 1L);

            assertThat(result).isNotNull();
            verify(reportRepository).save(report);
        }

        @Test
        void setReportAsRejectedUpdatesStatusAndSaves() {
            when(reportRepository.findById(1L)).thenReturn(Optional.of(report));
            when(userRepository.findById(1L)).thenReturn(Optional.of(user));
            when(reportRepository.save(any(Report.class))).thenAnswer(i -> i.getArgument(0));

            var result = serviceUnderTest.setReportAsRejected(1L, 1L);

            assertThat(result).isNotNull();
            verify(reportRepository).save(report);
        }
    }
}
