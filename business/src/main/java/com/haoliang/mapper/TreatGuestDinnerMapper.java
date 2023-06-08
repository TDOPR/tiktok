package com.haoliang.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.haoliang.model.TreatGuestDinner;
import com.haoliang.model.condition.TiktokTaskCondition;
import com.haoliang.model.condition.TreatGuestDinnerCondition;
import com.haoliang.model.vo.TreatGuestDinnerVO2;
import org.apache.ibatis.annotations.Param;

public interface TreatGuestDinnerMapper  extends BaseMapper<TreatGuestDinner> {
    Page<TreatGuestDinnerVO2> page(Page<TreatGuestDinner> page,@Param("param") TreatGuestDinnerCondition searchParam);
}
