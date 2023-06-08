package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.haoliang.model.WalletUsdLogs;
import com.haoliang.model.dto.DateSection;
import com.haoliang.model.vo.WalletUsdLogVO;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/3/5 18:06
 **/
public interface WalletUsdLogsMapper extends BaseMapper<WalletUsdLogs> {

    BigDecimal sumLockTal();
    DateSection getDateSection(Integer userId);

    BigDecimal sumAmountByAndUserIdAndTypeInAndDateRange(@Param("userId") Integer userId,@Param("typeList") List<Integer> depositTypeList, @Param("beginDate") LocalDate beginDate,@Param("endDate") LocalDate endDate);

    List<WalletUsdLogs> sumAmountGroupByType();

    List<WalletUsdLogs> sumYesterdayAmountGroupByType();

    BigDecimal sumYesterdayTotalAmountByType(Integer type);

    BigDecimal sumAmountByType(Integer type);

    Page<WalletUsdLogVO> mypage(Page<WalletUsdLogs> page, Integer userId, Integer type, LocalDate beginDate, LocalDate endDate);

    BigDecimal sumToDayTotalAmountByType(Integer value);

    List<WalletUsdLogs> sumAmountGroupByTypeAndParentId(Integer userId);

    List<WalletUsdLogs> sumYesterdayAmountGroupByTypeAndParentId(Integer userId);
}
