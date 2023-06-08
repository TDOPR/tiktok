package com.haoliang.config;

import com.alibaba.fastjson.JSONObject;
import com.haoliang.common.config.IgnoreSecurityPropetties;
import com.haoliang.security.JwtAuthenticationTokenFilter;
import com.haoliang.security.MyAuthenticationEntryPoint;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.annotation.web.configurers.ExpressionUrlAuthorizationConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

import javax.annotation.Resource;


/**
 * @author Dominick Li
 * @description Security 配置类
 **/
@SuppressWarnings("SpringJavaAutowiringInspection")
@Slf4j
@Configuration
@EnableWebSecurity
@EnableGlobalMethodSecurity(prePostEnabled = true)
public class WebSecurityConfig extends WebSecurityConfigurerAdapter {

    @Autowired
    private MyAuthenticationEntryPoint unauthorizedHandler;

    @Resource
    private UserDetailsService userDetailsService;

    @Autowired
    private IgnoreSecurityPropetties ignoreSecurityPropetties;

    @Bean
    @Override
    public AuthenticationManager authenticationManagerBean() throws Exception {
        return super.authenticationManagerBean();
    }

    @Autowired
    public void configureAuthentication(AuthenticationManagerBuilder authenticationManagerBuilder) throws Exception {
        authenticationManagerBuilder
                // 设置 UserDetailsService
                .userDetailsService(this.userDetailsService)
                // 使用 BCrypt 进行密码的 hash
                .passwordEncoder(passwordEncoder());
    }

    /**
     * 装载 BCrypt 密码编码器
     *
     * @return
     */
    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

    @Bean
    public JwtAuthenticationTokenFilter authenticationTokenFilterBean() throws Exception {
        return new JwtAuthenticationTokenFilter();
    }

    /**
     * token请求授权
     *
     * @param httpSecurity
     * @throws Exception
     */
    @Override
    protected void configure(HttpSecurity httpSecurity) throws Exception {
        String[] all = new String[ignoreSecurityPropetties.getAll().size()];
        String[] get = new String[ignoreSecurityPropetties.getGet().size()];
        String[] post = new String[ignoreSecurityPropetties.getPost().size()];
        ignoreSecurityPropetties.getGet().toArray(get);
        ignoreSecurityPropetties.getPost().toArray(post);
        ignoreSecurityPropetties.getAll().toArray(all);
        log.info("忽略认证的Get接口名称={}", JSONObject.toJSON(ignoreSecurityPropetties.getGet()));
        log.info("忽略认证的Post接口名称={}", JSONObject.toJSON(ignoreSecurityPropetties.getPost()));
        log.info("忽略认证不限制请求类型的接口名称={}", JSONObject.toJSON(ignoreSecurityPropetties.getAll()));

        httpSecurity
                // we don't need CSRF because our token is invulnerable
                .csrf().disable()
                .cors().and() // 跨域
                .headers().frameOptions().disable().and()
                .exceptionHandling().authenticationEntryPoint(unauthorizedHandler).and()
                // don't create session
                .sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS).and()
                .authorizeRequests();

        ExpressionUrlAuthorizationConfigurer<HttpSecurity>.ExpressionInterceptUrlRegistry authorizeRequest = httpSecurity.authorizeRequests();
        if (all.length > 0) {
            authorizeRequest.antMatchers(all).permitAll();
        }
        if (get.length > 0) {
            authorizeRequest.antMatchers(HttpMethod.GET, get).permitAll();
        }
        if (post.length > 0) {
            authorizeRequest.antMatchers(HttpMethod.POST, post).permitAll();
        }
        // 其它接口都要认证
        authorizeRequest.anyRequest().authenticated();

        // Custom JWT based security filter
        // 将token验证添加在密码验证前面
        httpSecurity.addFilterBefore(authenticationTokenFilterBean(), UsernamePasswordAuthenticationFilter.class);

        // disable page caching
        httpSecurity.headers().cacheControl();
    }


}
