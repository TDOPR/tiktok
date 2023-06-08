package com.haoliang.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.util.CurrencyExchangeRatesUtils;
import com.haoliang.pay.enums.FiatTypeEnum;
import com.haoliang.mapper.CurrencyExchangeRatesMapper;
import com.haoliang.model.CurrencyExchangeRates;
import com.haoliang.service.CurrencyExchangeRatesService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @Description
 * @CreateTime 2023/4/17 19:35
 **/
@Slf4j
@Service
public class CurrencyExchangeRatesServiceImpl extends ServiceImpl<CurrencyExchangeRatesMapper, CurrencyExchangeRates> implements CurrencyExchangeRatesService {

    private static final String SOURCE_NAME = "USD";

    private static final List<String> COIN_UTIL_LIST = FiatTypeEnum.getCoinUtilList();

    @Override
    public void updateExchangeRates() {
        JSONObject result = CurrencyExchangeRatesUtils.getNow(SOURCE_NAME, COIN_UTIL_LIST);
        if (result == null) {
            return;
        }
        CurrencyExchangeRates currencyExchangeRates;
        List<CurrencyExchangeRates> saveList = new ArrayList<>();
        if (result.containsKey("success") && result.getBoolean("success")) {
            LocalDateTime now = LocalDateTime.now();
            JSONObject quotes = result.getJSONObject("quotes");
            for (String coinUtil : COIN_UTIL_LIST) {
                currencyExchangeRates = new CurrencyExchangeRates();
                currencyExchangeRates.setSource(SOURCE_NAME);
                currencyExchangeRates.setTarget(coinUtil);
                currencyExchangeRates.setExchangeRate(quotes.getBigDecimal(SOURCE_NAME + coinUtil));
                currencyExchangeRates.setUpdateTime(now);
                saveList.add(currencyExchangeRates);
            }
            this.saveOrUpdateBatch(saveList);
            log.error("调用插入最新汇率接口!");
        } else {
            log.error("调用最新汇率接口异常:{}",result.get("message"));
        }
    }

//    public void updateExchangeRates() {
//        JSONObject result = CurrencyExchangeRatesUtils.getNow(SOURCE_NAME, COIN_UTIL_LIST);
//        List<CurrencyExchangeRates> list = this.list();
//        Map<String, CurrencyExchangeRates> map = list.stream().collect(Collectors.toMap(CurrencyExchangeRates::getTarget, Function.identity()));
//        CurrencyExchangeRates currencyExchangeRates;
//        List<CurrencyExchangeRates> saveList = new ArrayList<>();
//        if (result.getBoolean("success")) {
//            LocalDateTime now = LocalDateTime.now();
//            JSONObject quotes = result.getJSONObject("quotes");
//            for (String coinUtil : COIN_UTIL_LIST) {
//                if (map.containsKey(coinUtil)) {
//                    currencyExchangeRates = map.get(coinUtil);
//                } else {
//                    currencyExchangeRates = new CurrencyExchangeRates();
//                    currencyExchangeRates.setSource(SOURCE_NAME);
//                    currencyExchangeRates.setTarget(coinUtil);
//                }
//                currencyExchangeRates.setExchangeRate(quotes.getBigDecimal(SOURCE_NAME + coinUtil));
//                currencyExchangeRates.setUpdateTime(now);
//                saveList.add(currencyExchangeRates);
//            }
//            this.saveOrUpdateBatch(saveList);
//            log.error("调用更新汇率接口!");
//        } else {
//            log.error("调用最新汇率接口异常!");
//        }
//    }

    @Override
    public CurrencyExchangeRates getExchangeRatesByType(String fiatName) {
        Page<CurrencyExchangeRates> page = this.page(new Page<>(1, 1),
                new LambdaQueryWrapper<CurrencyExchangeRates>()
                        .eq(CurrencyExchangeRates::getTarget, fiatName)
                        .orderByDesc(CurrencyExchangeRates::getUpdateTime)
        );
        return page.getRecords().get(0);
    }
}
