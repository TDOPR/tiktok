package com.haoliang.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.util.NumberUtil;
import com.haoliang.common.util.RandomUtil;
import com.haoliang.constant.TiktokConfig;
import com.haoliang.mapper.KLineDataMapper;
import com.haoliang.model.KLineData;
import com.haoliang.model.vo.KLineDataInfoVO;
import com.haoliang.model.vo.KLineDataVO;
import com.haoliang.model.vo.KLineNowDataVO;
import com.haoliang.service.KLineDataService;
import com.haoliang.utils.BigDecimalUtils;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Dominick Li
 * @Description 生成当天的K线数据
 * @CreateTime 2023/3/2 11:08
 **/
@Service
public class KLineDataServiceImpl extends ServiceImpl<KLineDataMapper, KLineData> implements KLineDataService {

    @Override
    public void insertNow(LocalDate localDate) {
        this.save(KLineData.builder()
                .createDate(localDate)
                .kopen(RandomUtil.generate(10, 20, 2, 100))
                .klow(RandomUtil.generate(10, 20, 2, 100))
                .khigh(RandomUtil.generate(21, 50, 2, 100))
                .kvol(new BigDecimal(RandomUtil.randomInt(900000, 1000000)))
                .kclose(RandomUtil.generate(10, 50, 2, 100))
                .build());
    }

    @Override
    public KLineNowDataVO getKLineNowData() {
        Page<KLineData> kLineDataPage = this.page(new Page<>(1, 2), new LambdaQueryWrapper<KLineData>().orderByDesc(KLineData::getCreateDate));

        //最新价
        BigDecimal now = kLineDataPage.getRecords().get(0).getKclose();
        BigDecimal rate = BigDecimal.ZERO;
        if (kLineDataPage.getRecords().size() >= 2) {
            //昨天
            BigDecimal yesterday = kLineDataPage.getRecords().get(1).getKclose();
            rate = BigDecimalUtils.divideSaveTwoDecimal(now.subtract(yesterday).multiply(new BigDecimal(100)), yesterday);
        }
        KLineNowDataVO kLineNowDataVO = new KLineNowDataVO(kLineDataPage.getRecords().get(0), rate + "%");
        return kLineNowDataVO;
    }

    @Override
    public BigDecimal getNowExchangeRate() {
        BigDecimal bigDecimal = this.baseMapper.getNowExchangeRate();
        return bigDecimal == null ? BigDecimal.ZERO : bigDecimal;
    }

    @Override
    public JsonResult<KLineDataInfoVO> getKLineData() {
        List<KLineData> list = this.list(new LambdaQueryWrapper<KLineData>()
                .orderByDesc(KLineData::getCreateDate));
        KLineData npwData = list.get(0);
        BigDecimal now = npwData.getKclose();
        BigDecimal rate = BigDecimal.ZERO;
        if (list.size() >= 2) {
            //昨天
            BigDecimal yesterday = list.get(1).getKclose();
            rate = now.subtract(yesterday).multiply(new BigDecimal(100)).divide(yesterday, TiktokConfig.NUMBER_OF_DIGITS, RoundingMode.HALF_UP);
        }
        List<KLineDataVO> kLineDataVOList = new ArrayList<>();
        list.forEach(kLineData -> kLineDataVOList.add(new KLineDataVO(kLineData)));
        return JsonResult.successResult(KLineDataInfoVO.builder()
                .list(kLineDataVOList)
                .close(NumberUtil.toPlainString(npwData.getKclose()))
                .vol(NumberUtil.toPlainString(npwData.getKvol()))
                .upDownRange(NumberUtil.toPlainString(rate))
                .build());
    }

    @Override
    public JsonResult insertTestData() {
        LocalDate localDate = LocalDate.of(2023, 2, 1);
        LocalDate now = LocalDate.now();
        Integer i = 0;
        while (localDate.isBefore(now)) {
            System.out.println(i);
            i++;
            insertNow(localDate);
            localDate = localDate.plusDays(1);
        }
        return JsonResult.successResult();
    }

    @Override
    public JsonResult addAndEdit(KLineData kLineData) {
        KLineData exists = this.getOne(new LambdaQueryWrapper<KLineData>().eq(KLineData::getCreateDate, kLineData.getCreateDate()));
        if (exists != null && !exists.getCreateDate().equals(LocalDate.now())) {
            return JsonResult.failureResult(kLineData.getCreateDate() + "属于历史数据，不能修改！");
        }
        this.saveOrUpdate(kLineData);
        return JsonResult.successResult();
    }
}
