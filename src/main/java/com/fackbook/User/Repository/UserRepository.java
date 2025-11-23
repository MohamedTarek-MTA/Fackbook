package com.fackbook.User.Repository;

import com.fackbook.User.Entity.User;
import com.fackbook.User.Enum.Gender;
import com.fackbook.User.Enum.Role;
import com.fackbook.User.Enum.Status;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface UserRepository extends JpaRepository<User,Long> {
    Optional<User> findByEmail(String email);
    Optional<User> findByPhone(String phone);
    Page<User> findByNameContainingIgnoreCase(String name, Pageable pageable);
    Page<User> findByAddressContainingIgnoreCase(String address,Pageable pageable);
    Page<User> findByRole(Role role,Pageable pageable);
    Page<User> findByGender(Gender gender,Pageable pageable);
    Page<User> findByStatus(Status status,Pageable pageable);
}
