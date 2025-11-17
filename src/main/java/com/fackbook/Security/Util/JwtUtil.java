package com.fackbook.Security.Util;

import com.fackbook.Configuration.JwtConfig;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import lombok.RequiredArgsConstructor;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

import java.security.Key;
import java.util.Base64;
import java.util.Date;
import java.util.function.Function;

@Component
@RequiredArgsConstructor
public class JwtUtil {
    private final JwtConfig jwtConfig;
    private static final Logger logger =  LoggerFactory.getLogger(JwtUtil.class);
    public String generateToken(Long id,String email,String role,String status){
        long expirationTime = jwtConfig.getExpiration();
        try{
            String token = Jwts.builder()
                    .setSubject(email)
                    .claim("id",id)
                    .claim("role",role)
                    .claim("status",status)
                    .setIssuedAt(new Date())
                    .setExpiration(new Date(System.currentTimeMillis()+expirationTime))
                    .signWith(getSigningKey(), SignatureAlgorithm.HS512)
                    .compact();
            logger.info("Token Generated Successfully !");
            return token;
        }
        catch (Exception e){
            logger.error("Token Is Not Generated !\n"+e.getMessage(),e);
            return null;
        }

    }
    private Key getSigningKey(){
        byte[] keyBytes = Base64.getDecoder().decode(jwtConfig.getSecretKey());
        return Keys.hmacShaKeyFor(keyBytes);
    }
    private <T> T extractClaim(String token ,Function <Claims,T> claimsResolver){
        Claims claims = Jwts.parser()
                .setSigningKey(getSigningKey())
                .parseClaimsJws(token)
                .getBody();
        return claimsResolver.apply(claims);
    }
    public String extractUsername(String token){
        return extractClaim(token,Claims::getSubject);
    }
    private Date extractExpiration(String token){
        return extractClaim(token,Claims::getExpiration);
    }
    private boolean isTokenExpired(String token){
        return extractExpiration(token).before(new Date());
    }
    public boolean validateToken(String token, UserDetails userDetails){
        return extractUsername(token).equals(userDetails.getUsername()) && !isTokenExpired(token);
    }

}
