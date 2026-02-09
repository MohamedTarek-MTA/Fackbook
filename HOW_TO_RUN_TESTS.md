# How to Run Test Cases

This project uses **JUnit 5** and **Mockito** for unit tests. Tests are in `src/test/java` under the same package structure as the main code.

---

## Run All Tests

From the project root (where `pom.xml` is):

**Using Maven Wrapper (recommended if `mvn` is not in PATH):**

```cmd
mvnw.cmd test
```

**Using Maven (if installed):**

```bash
mvn test
```

On Windows PowerShell, from project root:

```powershell
.\mvnw.cmd test
```

---

## Run a Single Test Class

```bash
mvn test -Dtest=UserServiceTest
```

Examples:

```bash
mvn test -Dtest=GroupServiceTest
mvn test -Dtest=FriendServiceTest
mvn test -Dtest=RequestServiceTest
```

---

## Run a Single Test Method

```bash
mvn test -Dtest=UserServiceTest#getUserById_returnsUserDTOWhenFound
```

Use the exact class name and method name (replace `#` with `#` and use the full method name as shown in your IDE).

---

## Run Tests in a Package

```bash
mvn test -Dtest="com.fackbook.**.*Test"
```

This runs all classes whose name ends with `Test` under `com.fackbook`.

---

## Run Tests from Your IDE

- **IntelliJ IDEA**: Right‑click `src/test/java` or a test class → **Run 'Tests in...'** or **Run 'TestClassName'**.
- **Eclipse**: Right‑click the test class → **Run As** → **JUnit Test**.
- **VS Code / Cursor**: Use the **Testing** view or run the default **Test** task if configured for Java.

---

## Test Structure

| Package / Class | What it tests |
|-----------------|----------------|
| `TestFixtures` | Shared test data (users, groups, requests, etc.) |
| `UserServiceTest` | UserService: getById, getByEmail, exists, status/role changes, pagination |
| `FriendServiceTest` | FriendService: friendships, approve request, end friendship |
| `GroupServiceTest` | GroupService: CRUD, getByName, status changes |
| `GroupMemberServiceTest` | GroupMemberService: members, handleGroupMembership, toGroupMember |
| `RequestServiceTest` | RequestService: create, get, accept/reject, list by type |
| `ShareServiceTest` | ShareService: getShare, getShares, createNewShare |
| `ReactServiceTest` | ReactService: react, getReactionSummary |
| `ReportServiceTest` | ReportService: getReport, getReports, setReportAsAccepted/Rejected |
| `AccessibilityServiceTest` | AccessibilityService: validateVisibility, validateModeration |
| `CustomUserDetailsServiceTest` | CustomUserDetailsService: loadUserByUsername |
| `NotificationServiceTest` | NotificationService: sendNotificationViaRequest, sendNotificationViaReact |

---

## Skipping Tests (e.g. for Build)

To compile and package without running tests:

```bash
mvn clean install -DskipTests
```

To skip only test execution but still compile tests:

```bash
mvn clean install -Dmaven.test.skip=false -DskipTests
```

---

## Optional: Test Profile

If you need a dedicated profile (e.g. to disable Kafka or use H2 in tests), add to `src/main/resources/application.yml`:

```yaml
spring:
  profiles:
    active: test
```

And create `src/test/resources/application-test.yml` with test-specific settings. The default `mvn test` does not load the full Spring context for the unit tests in this project (they use `@ExtendWith(MockitoExtension.class)` and mocks only), so no profile is required for the current tests.
