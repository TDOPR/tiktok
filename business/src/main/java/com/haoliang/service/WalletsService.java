package com.haoliang.service;

import com.baomidou.mybatisplus.core.toolkit.support.SFunction;
import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.common.model.JsonResult;
import com.haoliang.enums.FlowingActionEnum;
import com.haoliang.enums.TttLogTypeEnum;
import com.haoliang.enums.UsdLogTypeEnum;
import com.haoliang.model.TiktokTaskPrices;
import com.haoliang.model.Wallets;
import com.haoliang.model.dto.AmountDTO;
import com.haoliang.model.dto.AppUsersAmountDTO;
import com.haoliang.model.dto.BuyVipDTO;
import com.haoliang.model.vo.MyWalletsVO;
import com.haoliang.model.vo.TttWalletInfoVO;
import com.haoliang.model.vo.VipLevelVO;
import com.haoliang.pay.enums.CoinUnitEnum;

import java.math.BigDecimal;
import java.util.List;

public interface WalletsService extends IService<Wallets> {

    /**
     * 获取我的钱包信息
     *
     * @return
     */
    JsonResult<MyWalletsVO> getMyWallet();

    /**
     * 查询钱包对象
     *
     * @param userId  根据用户Id查询
     * @param columns 需要查询的字段
     * @return
     */
    Wallets selectColumnsByUserId(Integer userId, SFunction<Wallets, ?>... columns);

    /**
     * 更新钱包Usd余额
     *
     * @param amount            需要加或减的金额
     * @param userId            用户Id
     * @param flowingActionEnum 收入或支出
     * @param usdLogTypeEnum    流水类型
     * @return 执行结果
     */
    boolean updateUsdWallet(BigDecimal amount, Integer userId, FlowingActionEnum flowingActionEnum, UsdLogTypeEnum usdLogTypeEnum);

    /**
     * 更新钱包Usd余额
     *
     * @param amount            需要加或减的金额
     * @param userId            用户Id
     * @param flowingActionEnum 收入或支出
     * @param usdLogTypeEnum    流水类型
     * @param coinUnitEnum      充值和提现的渠道
     * @return 执行结果
     */
    boolean updateUsdWallet(BigDecimal amount, Integer userId, FlowingActionEnum flowingActionEnum, UsdLogTypeEnum usdLogTypeEnum, CoinUnitEnum coinUnitEnum);

    /**
     * 更新钱包余额
     *
     * @param amount            需要加或减的金额
     * @param userId            用户Id
     * @param flowingActionEnum 收入或支出
     * @param tttLogTypeEnum    流水类型
     * @param zero    是否为零撸流水 true=零撸 false=非零撸
     * @return 执行结果
     */
    boolean updateTttWallet(BigDecimal amount, Integer userId, FlowingActionEnum flowingActionEnum, TttLogTypeEnum tttLogTypeEnum, boolean zero);

    /**
     * 通过数据库字段计算的方式修改钱包余额
     *
     * @param userId            用户Id
     * @param amount            修改的金额
     * @param flowingActionEnum 增加或减少
     * @return
     */
    boolean lookUsdUpdateWallets(Integer userId, BigDecimal amount, FlowingActionEnum flowingActionEnum);

    /**
     * 通过数据库字段计算的方式修改钱包余额
     *
     * @param userId            用户Id
     * @param amount            修改的金额
     * @param flowingActionEnum 增加或减少
     * @return
     */
    boolean lookUpdateWallets(Integer userId, BigDecimal amount, FlowingActionEnum flowingActionEnum);

    /**
     * 取消冻结用户金额
     *
     * @param userId 用户Id
     * @param amount 需要解冻的金额
     * @param coinUnitEnum 提现渠道
     * @return
     */
    boolean unFrozenAmount(Integer userId, BigDecimal amount,Long usdLogId,CoinUnitEnum coinUnitEnum);

    /**
     * 冻结金额
     *
     * @param userId 用户Id
     * @param amount 冻结金额
     * @param coinUnitEnum 提现的渠道
     * @return 冻结的流水Id
     */
    Long frozenAmount(Integer userId, BigDecimal amount,CoinUnitEnum coinUnitEnum);

    /**
     * 重冻结金额中扣除指定金额
     *
     * @param userId 用户Id
     * @param amount 扣减的费用
     */
    boolean reduceFrozenAmount(Integer userId, BigDecimal amount,Long usdLogId);

    /**
     * 为用户绑定一条区块链地址
     *
     * @param networdName 需要分配的网络名称
     * @return
     */
    JsonResult getBlockAddress(String networdName);

    /**
     * 购买次数包
     *
     * @param userId           用户Id
     * @param tiktokTaskPrices 购买的套餐包
     */
    void buyTaskNumPackage(Integer userId, TiktokTaskPrices tiktokTaskPrices);

    /**
     * 扣减任务包次数
     *
     * @param userId 用户Id
     * @param num    扣减的数量
     */
    void reduceHasTaskNum(Integer userId, Integer num);

    JsonResult<List<VipLevelVO>> getVipList();

    /**
     * 购买VIP套餐
     *
     * @return
     */
    JsonResult buyVip(BuyVipDTO buyVipDTO);

    /**
     * 获取ttt钱包行
     *
     * @return
     */
    JsonResult<TttWalletInfoVO> getTttWalletInfo();

    /**
     * ttt账户转换到usd账号
     *
     * @param amountDTO
     * @return
     */
    JsonResult tttConversionUsd(AmountDTO amountDTO);

    List<AppUsersAmountDTO> findHoldingCoinUserInfo();
}
