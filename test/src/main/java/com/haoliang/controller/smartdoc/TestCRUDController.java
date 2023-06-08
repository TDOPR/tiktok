package com.haoliang.controller.smartdoc;

import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.haoliang.common.base.BaseCondition;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.IntIdListDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.mapper.TestMapper;
import com.haoliang.model.Test;
import com.haoliang.service.TestService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import java.util.List;

/**
 * CRUD通用功能测试
 * 建议在Idea里面安装 Mybatis X插件提高开发效率
 *
 * @author Dominick Li
 * @CreateTime 2022/10/21 17:48
 **/
@RestController
@RequestMapping("/test")
public class TestCRUDController {

    @Autowired
    private TestService testService;

    @Resource
    private TestMapper testMapper;

    /**
     * 下面是TestService  继承 IService 默认提供的接口
     * testService.list 查询列表
     * testService.page 分页查询
     * testService.remove 删除数据
     * testService.save 插入数据
     * testService.saveOrUpdate 添加或修改数据
     * testService.saveOrUpdateBatch 批量添加或修改
     * testService.getById 根据id查询数据
     * testService.getOne 根据条件查询一条数据
     */

    /**
     * 分页查询
     */
    @GetMapping("/")
    public JsonResult findAll(PageParam<Test, BaseCondition> pageParam) {
        IPage<Test> iPage = testService.page(pageParam.getPage(), pageParam.getSearchParam().getQueryWrapper());
        return JsonResult.successResult(new PageVO<>(iPage));
    }

    /**
     * 列表查询 list
     */
    @GetMapping("/list")
    public JsonResult list() {
        List<Test> testList = testMapper.findAllByName("user");
        //List<Test> testList=testService.list(new LambdaQueryWrapper<Test>().eq(Test::getName,"user")); //等值查询
        //List<Test> testList = testService.list(new LambdaQueryWrapper<Test>().like(Test::getName, "user")); //模糊查询
        return JsonResult.successResult(testList);
    }


    /**
     * 根据Id查询
     *
     * @param id 用户Id
     */
    @GetMapping("/{id}")
    public JsonResult findById(@PathVariable Integer id) {
        //testService.getOne(new LambdaQueryWrapper<Test>().eq(Test::getId,id));//根据条件查询
        return JsonResult.successResult(testService.getById(id));
    }

    /**
     * 添加或保存数据
     */
    @PostMapping("/")
    public JsonResult save(@RequestBody Test test) {
        return JsonResult.build(testService.saveOrUpdate(test));
    }

    /**
     * 修改数据
     */
    @PostMapping("/update")
    public JsonResult update(@RequestBody Test test) {
        //根据Id只修改name字段数据
        UpdateWrapper<Test> wrapper = Wrappers.update();
        wrapper.lambda()
                .set(Test::getName, test.getName())
                .eq(Test::getId, test.getId());
        return JsonResult.build(testService.update(wrapper));
    }

    /**
     * 删除数据
     */
    @PostMapping("/deletebyIds")
    public JsonResult deletebyIds(@RequestBody IntIdListDTO intIdListDTO) {
        //testService.removeById(1);//根据Id删除数据
        //testService.remove(new LambdaQueryWrapper<Test>().eq(Test::getName, "张三")); //根据条件删除数据
        return JsonResult.build(testService.removeByIds(intIdListDTO.getIdList()));
    }

}
