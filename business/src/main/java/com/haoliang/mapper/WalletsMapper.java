package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.haoliang.model.Wallets;
import com.haoliang.model.dto.AppUsersAmountDTO;
import com.haoliang.model.dto.UserWalletsDTO;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.util.List;

public interface WalletsMapper extends BaseMapper<Wallets> {

    /**
     * 查询大于指定等级的供应商信息
     * @param level 代理商等级
     * @param principalAmount 获取动态收益最低托管金额
     */
    List<UserWalletsDTO> selectUserWalletsDTOListByUserLevelGtAndPrincipalAmountGe(@Param("level") Integer level,@Param("principalAmount") BigDecimal principalAmount);

    /**
     * 往钱包中充值
     * @param userId 用户Id
     * @param amount 充值的金额
     */
    int lockUpdateAddWallet(@Param("userId")Integer userId,@Param("amount") BigDecimal amount);

    /**
     * 往钱包中取出金额
     * @param userId 用户Id
     * @param amount 取出的金额
     */
    int lockUpdateReduceWallet(@Param("userId")Integer userId,@Param("amount") BigDecimal amount);

    int frozenAmount(@Param("userId")Integer userId,@Param("amount") BigDecimal amount);

    int unFrozenAmount(@Param("userId")Integer userId,@Param("amount") BigDecimal amount);

    int reduceFrozenAmount(@Param("userId")Integer userId,@Param("amount") BigDecimal amount);

    int lockUpdateAddUsdWallet(@Param("userId")Integer userId,@Param("amount") BigDecimal amount);

    int lockUpdateReduceUsdWallet(@Param("userId")Integer userId,@Param("amount") BigDecimal amount);

    void buyTaskNumPackage(@Param("userId")Integer userId,@Param("amount") Integer amount, @Param("num")Integer num);

    void reduceHasTaskNum(@Param("userId")Integer userId, @Param("num")Integer num);

    int tttConversionUsd(@Param("userId")Integer userId,@Param("tttAmount") BigDecimal amount,@Param("usdAmount")  BigDecimal exchangeUsd);

    List<AppUsersAmountDTO> findHoldingCoinUserInfo();

    BigDecimal sumUsdAmount();

    BigDecimal sumTttAmount();
}
