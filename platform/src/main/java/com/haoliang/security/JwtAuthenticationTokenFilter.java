package com.haoliang.security;

import com.haoliang.common.constant.CacheKeyPrefixConstants;
import com.haoliang.common.constant.SystemConstants;
import com.haoliang.common.util.JwtTokenUtil;
import com.haoliang.service.SysMenuService;
import io.jsonwebtoken.Claims;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * @author Dominick Li
 * @description toekn认证
 **/
@Component
@Slf4j
public class JwtAuthenticationTokenFilter extends OncePerRequestFilter {

    @Autowired
    private SysMenuService sysMenuService;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        final String tokenKey = request.getHeader(SystemConstants.TOKEN_NAME);
        if (tokenKey != null) {
            Claims claims = JwtTokenUtil.getTokenClaim(tokenKey);
            if (claims != null && SecurityContextHolder.getContext().getAuthentication() == null) {
                UserDetails userDetails = null;
                if (!claims.containsKey(SystemConstants.ROLE_ID)) {
                    userDetails = new MyUserDetail();
                } else {
                    userDetails = new MyUserDetail(claims.get("username", String.class), claims.get(SystemConstants.ROLE_CODE, String.class), sysMenuService.findAuthorityByRoleId(claims.get(SystemConstants.ROLE_ID, Integer.class)));
                }
                UsernamePasswordAuthenticationToken authentication = new UsernamePasswordAuthenticationToken(userDetails, null, userDetails.getAuthorities());
                authentication.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
                logger.debug("authorication user: " + claims.getSubject() + ", setting security context");
                SecurityContextHolder.getContext().setAuthentication(authentication);
            }
        }
        filterChain.doFilter(request, response);
    }
}
