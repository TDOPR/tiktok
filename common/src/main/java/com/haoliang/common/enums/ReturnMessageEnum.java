package com.haoliang.common.enums;

import com.haoliang.common.model.ThreadLocalManager;
import com.haoliang.common.util.MessageUtil;
import lombok.Getter;

@Getter
public enum ReturnMessageEnum {

    OK("ok", "操作成功！"),
    FAILED("failed", "操作失败！"),
    ERROR("error", "系统内部错误，请联系管理员！"),
    PARAM_CANNOT_BE_NULL("param_is_empty", "参数不能为空！"),
    TAKS_NUM_IS_ZERO("task_num_is_zero", "任务已被接完！"),
    VERIFICATION_CODE_EXPIRE("code_expire", "验证码过期！"),
    VERIFICATION_CODE_ERROR("code_error", "验证码错误！"),
    EMAIL_NOT_EXISTS("email_not_exists", "邮箱号未注册账号！"),
    EMAIL_EXISTS("email_esists", "邮箱号已被注册！"),
    TIKTOK_USERNAME_EXISTS("tiktok_username_exists", "tiktik用户Id已被绑定！"),

    MOBILE_EXISTS("mobile_exists", "手机号已被绑定！"),
    SEND_EMAIL_ERROR("send_eamil_error", "发送验证码失败！"),
    ACCOUNT_DISABLED("account_disabled", "账号已被禁用！"),
    PASSWORD_ERROR("pwd_error", "密码错误！"),
    NO_SUPPORT_EMPJI("no_support_empji", "个人信息里不能包含表情包!"),
    ORIGINAL_PASSWORD_ERROR("original_pwd_error", "原密码错误！"),
    INVITE_CODE_ERROR("inviteCode_error", "邀请码错误，请确认无误后再输入！"),
    BLOCK_ADDRESS_EMPTY("block_address_empty", "没有可用的充值地址"),
    AMOUNT_EXCEEDS_BALANCE("amount_exceeds_balance", "余额不足！"),
    TASK_NUM_BALANCE("task_num_exceeds", "任务数超出可用次数！"),
    MIN_AMOUNT("min_amount", "托管金额存入不能低于{0}$！"),
    WITHDRAW_MIN_AMOUNT("withdraw_min_amount", "USDT提现的金额未达到提现最低额度{0}$！"),
    UB_SUPPORT_NETWORD("un_support_netword", "不支持的网络类型"),
    REPEATED_SUBMISSION("repeated_submission", "请求太频繁，请稍后再试！"),
    RECHARGE_ERROR("recharge_error", "调用支付接口异常!"),
    UNAUTHORIZED("unauthorized", "登录已超时，请重新登录！"),
    ACCOUNT_EXISTS("account_esists", "账号已注册！"),
    ACCOUNT_NOT_EXISTS("account_not_esists", "账号不存在！"),
    ACCOUNT_LOCK("account_lock", "账号已被锁定{0}分钟，请稍后重试！"),
    MENU_EXISTS_USE("menu_exists_use", "菜单已被角色使用，不能删除！"),
    ROLE_EXISTS_USE("role_exists_use", "角色已被用户使用，不能删除！"),
    ROLE_NAME_EXISTS("role_name_exists", "角色名称不能重复！"),
    ROLE_CODE_EXISTS("role_code_exists", "角色编码不能重复！"),
    VIP_NOT_PURCHASED("vip_not_purchased","购买vip后可领取！" ),
    PLEASE_UPLOAD_IMG("please_upload_img","请上传截图后再提交审核！" ),
    SEND_SMS_FAIL("send_sms_error","发送短信失败!" ),

    TASK_NUM_LIMIT("task_num_limit", "当天接单的已经达到上限！"),
    INSUFFICIENT_LIMIT("insufficient_limit", "余额不足，请购买vip套餐后再接单！"),
    INPUT_USER_ID_ERROR("input_user_id_error","输入的用户ID有误！" ),
    MIN_USD_ERROR("min_usd_error","换算后的USD必须大于 0.01$！"),
    UPDATE_PWD_TIME_LIMIT("update_pwd_time_limit","修改密码后间隔24小时才可进行提款操作！"),
    WITHDRAW_COUNT_LIMIT("withdraw_count_limit","同一账号同个钱包地址24小时内提币次数上限为3次！")
    ;

    /**
     * 国际化信息文件里的Key前缀
     */
    private final static String prefix = "return.";

    /**
     * 返回的国际化key信息
     */
    private String key;

    private String cnMessage;

    ReturnMessageEnum(String key, String cnMessage) {
        this.key = prefix + key;
        this.cnMessage = cnMessage;
    }

    @Override
    public String toString() {
        return MessageUtil.get(key, ThreadLocalManager.getLanguage());
    }

    /**
     * 替换占位符中的参数
     *
     * @param params 需要替换的参数值 长度可变
     */
    public String setAndToString(Object... params) {
        return MessageUtil.get(key, params, ThreadLocalManager.getLanguage());
    }

}
