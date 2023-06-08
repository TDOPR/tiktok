package com.haoliang.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.haoliang.model.CurrencyExchangeRates;

public interface CurrencyExchangeRatesService  extends IService<CurrencyExchangeRates> {

    /**
     * 更新汇率
     */
    void updateExchangeRates();

    CurrencyExchangeRates getExchangeRatesByType(String fiatName);

}
