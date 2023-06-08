package com.haoliang.controller;

import com.alibaba.fastjson.JSONObject;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.util.NumberUtil;
import com.haoliang.model.dto.AmountDTO;
import com.haoliang.model.vo.KLineDataInfoVO;
import com.haoliang.model.vo.TttWalletInfoVO;
import com.haoliang.service.KLineDataService;
import com.haoliang.service.WalletsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * @author Dominick Li
 * @Description ttt币管理
 * @CreateTime 2023/3/6 15:52
 **/
@RestController
@RequestMapping("/ttt")
public class TttController {

    @Autowired
    private WalletsService walletsService;

    @Autowired
    private KLineDataService kLineDataService;

    /**
     * 主页钱包信息
     */
    @GetMapping
    public JsonResult<TttWalletInfoVO> index() {
        return walletsService.getTttWalletInfo();
    }

    /**
     * 获取主页展示的K线数据
     */
    @GetMapping("/getKLineData")
    public JsonResult<KLineDataInfoVO> getKLineData() {
        return kLineDataService.getKLineData();
    }

    /**
     * TTT账户转换到USD
     */
    @PostMapping("/conversion")
    public JsonResult conversion(@RequestBody AmountDTO amountDTO) {
        return walletsService.tttConversionUsd(amountDTO);
    }

    /**
     * 获取最新汇率
     */
    @GetMapping("/getNowExchangeRate")
    public JsonResult getNowExchangeRate() {
        JSONObject object = new JSONObject();
        object.put("nowExchangeRate", NumberUtil.toPlainString(kLineDataService.getNowExchangeRate()));
        return JsonResult.successResult(object);
    }

}
