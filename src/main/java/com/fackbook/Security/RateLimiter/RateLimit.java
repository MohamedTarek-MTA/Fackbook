package com.fackbook.Security.RateLimiter;

import java.lang.annotation.*;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface RateLimit {
    int maxRequests() default 5;
    long timeWindowMs() default 60000;    // time window is 60 seconds
}
