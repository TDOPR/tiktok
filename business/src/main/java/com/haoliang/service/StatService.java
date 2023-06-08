package com.haoliang.service;

import com.haoliang.common.base.DefaultCondition;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.model.StatBalance;
import com.haoliang.model.StatDynamicAndStatic;
import com.haoliang.model.StatInputAndOutput;
import com.haoliang.model.StatUser;
import com.haoliang.model.vo.BusinessVO;


/**
 * @author Dominick Li
 * @Description 数据统计
 * @CreateTime 2023/3/31 16:20
 **/

public interface StatService {

    /**
     * 统计数据
     */
    void stat();

    /**
     * 分页查看用户数据
     */
    JsonResult<PageVO<StatUser>> statUserPage(PageParam<StatUser, DefaultCondition> pageParam);

    /**
     * 出入金管理
     */
    JsonResult<PageVO<StatInputAndOutput>> inputAndOutPutPage(PageParam<StatInputAndOutput, DefaultCondition> pageParam);

    /**
     * 动静态管理
     * @param pageParam
     * @return
     */
    JsonResult dynamicAndStaticPage(PageParam<StatDynamicAndStatic, DefaultCondition> pageParam);

    /**
     * 余额持有管理
     * @param pageParam
     * @return
     */
    JsonResult balancePage(PageParam<StatBalance, DefaultCondition> pageParam);

    /**
     * 资金数据
     * @return
     */
    JsonResult fund();

    /**
     * 获取后台主页数据
     * @return
     */
    BusinessVO getAdminPanel();

    /**
     * 获取实时数据
     * @return
     */
    JsonResult userNowData();
}
