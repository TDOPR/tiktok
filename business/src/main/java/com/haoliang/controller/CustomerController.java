package com.haoliang.controller;

import com.haoliang.common.annotation.OperationLog;
import com.haoliang.common.constant.OperationAction;
import com.haoliang.common.constant.OperationModel;
import com.haoliang.common.model.JsonResult;
import com.haoliang.common.model.PageParam;
import com.haoliang.common.model.dto.UpdateStatusDTO;
import com.haoliang.common.model.vo.PageVO;
import com.haoliang.model.AppUsers;
import com.haoliang.model.condition.AppUsersCondition;
import com.haoliang.model.vo.AppUsersVO;
import com.haoliang.service.AppUserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * @author Dominick Li
 * @Description 客户管理
 * @CreateTime 2023/1/8 15:31
 **/
@RestController
@RequestMapping("/customer")
public class CustomerController {

    @Autowired
    private AppUserService appUserService;

    /**
     * 分页查询
     */
    @PostMapping("/pagelist")
    @PreAuthorize("hasAuthority('user:list')")
    public JsonResult<PageVO<AppUsersVO>> pageList(@RequestBody PageParam<AppUsers, AppUsersCondition> pageParam) {
        return appUserService.pageList(pageParam);
    }

    /**
     * 添加初始用户
     */
    @PostMapping
    @PreAuthorize("hasAuthority('user:add')")
    public JsonResult addUser(@Valid @RequestBody AppUsers appUsers) {
        return appUserService.addUser(appUsers);
    }

    /**
     * 用户画像
     */
    @GetMapping("/portrait")
    @PreAuthorize("hasAuthority('portrait:list')")
    public JsonResult portrait() {
        return appUserService.portrait();
    }

    /**
     * 查看客户下级
     */
    @GetMapping("/tree/{userId}")
    @PreAuthorize("hasAuthority('user:check:low')")
    public JsonResult findTree(@PathVariable Integer userId) {
        return appUserService.treeDiagram(userId);
    }

    /**
     * 设置为代理商
     */
    @PostMapping("/editProxyStatus")
    @OperationLog(module = OperationModel.ARTICLE, description = OperationAction.SET_PROXY)
    @PreAuthorize("hasAnyAuthority('user:proxy')")
    public JsonResult editProxyStatus(@RequestBody UpdateStatusDTO updateStatusDTO) {
        return appUserService.editProxyStatus(updateStatusDTO);
    }


}
