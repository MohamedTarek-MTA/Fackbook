package com.fackbook.Security.RateLimiter;

import com.fackbook.Exception.RateLimitException;
import com.fackbook.Security.Util.JwtUtil;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Aspect
@Component
@RequiredArgsConstructor
public class RateLimiterAspect {

    private static class RequestCounter {
        long lastWindowStart;
        int count;
    }

    private final JwtUtil jwtUtil;
    private final Map<String, RequestCounter> requestMap = new ConcurrentHashMap<>();

    @Around("@annotation(rateLimit)")
    public Object rateLimit(ProceedingJoinPoint joinPoint, RateLimit rateLimit) throws Throwable {
        String key = generateKey(joinPoint);

        RequestCounter counter = requestMap.computeIfAbsent(key, k -> new RequestCounter());
        long now = System.currentTimeMillis();

        synchronized (counter) {
            if (now - counter.lastWindowStart > rateLimit.timeWindowMs()) {
                counter.lastWindowStart = now;
                counter.count = 0;
            }

            if (counter.count < rateLimit.maxRequests()) {
                counter.count++;
            } else {
                throw new RateLimitException("Too many requests. Try again later.");
            }
        }

        return joinPoint.proceed();
    }

    private String generateKey(ProceedingJoinPoint joinPoint) {

        HttpServletRequest request =
                ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).getRequest();

        String method = joinPoint.getSignature().toShortString();

        String authHeader = request.getHeader("Authorization");
        String email = null;

        // Extract email from JWT safely
        if (authHeader != null && authHeader.startsWith("Bearer ")) {
            String token = authHeader.substring(7);
            try {
                email = jwtUtil.extractUsername(token);
            } catch (Exception ignored) {}
        }

        // Authenticated user → use email-based key
        if (email != null) {
            return "rate_limit:user:" + email + ":" + method;
        }

        // Not authenticated → fallback to IP-based key
        String ip = request.getRemoteAddr();
        if (ip == null) ip = "unknown";

        return "rate_limit:ip:" + ip + ":" + method;
    }
}
