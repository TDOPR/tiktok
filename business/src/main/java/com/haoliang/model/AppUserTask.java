package com.haoliang.model;

import com.baomidou.mybatisplus.annotation.TableName;
import com.haoliang.common.base.BaseModel;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * @author Dominick Li
 * @Description 用户接取的任务
 * @CreateTime 2023/2/24 15:20
 **/
@Data
@TableName("app_user_task")
public class AppUserTask extends BaseModel {

    /**
     * 用户Id
     */
    private Integer userId;

    /**
     * 任务Id
     */
    private Long taskId;

    /**
     * 任务状态  1=已接取  2=待审核  3=已驳回 4=已完成
     */
    private Integer status;

    /**
     * 待审需要的截图文件存储路径
     */
    private String imgUrl;

    /**
     * ttt奖励
     */
    private BigDecimal amount;

    /**
     * 对应使用的vip套餐Id列表,存在用户买了多个套餐的问题
     */
    private String  vipOrderIds;

    /**
     * 接单的vip等级
     */
    private Integer level;

    /**
     * 审核时间
     */
    private LocalDateTime auditTime;

}
