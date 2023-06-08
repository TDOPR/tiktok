package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.haoliang.model.WalletTttLogs;
import com.haoliang.model.dto.DateSection;
import com.haoliang.model.dto.TeamTaskDTO;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

public interface WalletTttLogsMapper extends BaseMapper<WalletTttLogs> {

    List<WalletTttLogs> sumByTypeList(@Param("userId")Integer userId, @Param("typeList")List<Integer> typeList);

    DateSection getDateSection(Integer userId);

    LocalDate getMinDate();

    BigDecimal sumTotalAmountByTypeAndDateBetween(@Param("type")Integer type, @Param("begin")LocalDate localDate, @Param("end")LocalDate end);

    BigDecimal sumTotalAmountByTypeInAndDateBetween(@Param("typeList")List<Integer> typeList, @Param("begin")LocalDate localDate, @Param("end")LocalDate end);

    BigDecimal sumTotalAmountByUserIdAndTypeIn(@Param("userId")Integer userId, @Param("typeList") List<Integer> dynamicTypeList);

    Page<WalletTttLogs> pageByMerge(Page<WalletTttLogs> page, @Param("userId") Integer userId, @Param("beginDate") LocalDate beginDate, @Param("endDate") LocalDate endDate, @Param("typeList") List<Integer> typeList);

    Page<WalletTttLogs> pageByMergeAndGroupByType(Page<WalletTttLogs> page, @Param("userId") Integer userId, @Param("typeList") List<Integer> typeList);

    Page<WalletTttLogs> pageByAllType(Page<WalletTttLogs> page, @Param("userId") Integer userId, @Param("beginDate") LocalDate beginDate, @Param("endDate") LocalDate endDate);

    BigDecimal sumAmountByAndUserIdAndTypeInAndDateRange(@Param("userId")Integer userId, @Param("typeList") List<Integer> typeList, @Param("beginDate") LocalDate beginDate, @Param("endDate")LocalDate endDate);

    BigDecimal getSumTaskEarningsByTypeIn(List<Integer> typeList);

    List<WalletTttLogs> sumByType(Integer userId);

    List<WalletTttLogs> sumYesterdayByType(Integer userId);

    List<TeamTaskDTO> getSumTaskEarningGroupByUser(List<Integer> typeList);

    BigDecimal sumTotalAmountByTypeList(List<Integer> typeList);


    BigDecimal sumYesterdayTotalAmountByTypeList(List<Integer> typeList);

    BigDecimal sumYesterdayTotalZeroAmount();

    BigDecimal sumTotalAmountByTypeListAndParentId(List<Integer> typeList, Integer userId);

    BigDecimal sumYesterdayTotalAmountByTypeListAndParentId(List<Integer> typeList, Integer userId);

    BigDecimal sumYesterdayTotalZeroAmountAndParentId(Integer userId);
}
