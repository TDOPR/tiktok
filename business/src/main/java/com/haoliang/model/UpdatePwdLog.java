package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 更新密码的记录表
 * @CreateTime 2023/5/16 11:16
 **/
@Data
@TableName("update_pwd_log")
@AllArgsConstructor
public class UpdatePwdLog {

    /**
     * 用户Id
     */
    @TableId
    private Integer userId;

    /**
     * 最后一次修改时间
     */
    private LocalDateTime lastmodifiedTime;

}
