package com.haoliang.common.util;

import com.alibaba.fastjson.JSONObject;
import lombok.NoArgsConstructor;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Dominick Li
 * @Description 汇率转换接口
 * @CreateTime 2023/4/17 19:12
 **/
@NoArgsConstructor
public class CurrencyExchangeRatesUtils {

    /**
     * https://apilayer.com/marketplace/currency_data-api#pricing
     * API身份凭证
     */
    public static final String ACCESS_KEY = "oX7YSQxBQL8fOp1JeKQJKL7MI5vJ92HY";

    public static JSONObject getNow(String source, List<String> fiatTypeList) {
        String fiatTypeStr = fiatTypeList.stream().collect(Collectors.joining(","));
        HashMap<String, String> header = new HashMap<>();
        header.put("apikey", ACCESS_KEY);
        String url=String.format("https://api.apilayer.com/currency_data/live?base=%s&symbols=%s", source, fiatTypeStr);
        String str = HttpUtil.get(url, header);
        return JSONObject.parseObject(str);
    }

    public static void main(String[] args) throws IOException {
        List<String> fiatTypeList = Arrays.asList("IDR","THB","VND");
        System.out.println(getNow("USD", fiatTypeList));
    }

}
