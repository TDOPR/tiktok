CREATE DATABASE IF NOT EXISTS tiktok;
use tiktok;

-- -------------------------------
-- Table structure for sys_role
-- -------------------------------
DROP TABLE IF EXISTS `sys_role`;
CREATE TABLE `sys_role`  (
  `id` int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
  `createTime` datetime(0) NULL DEFAULT NULL COMMENT '创建时间',
  `lastmodifiedTime` datetime(0) NULL DEFAULT NULL COMMENT '修改时间',
  `roleCode` varchar(255) NOT NULL DEFAULT ''  COMMENT '角色编码',
  `roleName` varchar(255) NOT NULL DEFAULT ''  COMMENT '角色名称',
  `enabled` tinyint NOT NULL DEFAULT 1 COMMENT '角色状态 1=可用 0=删除',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB  CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT = '角色表' ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for sys_channel
-- --------------------------------
DROP TABLE IF EXISTS `sys_channel`;
CREATE TABLE `sys_channel`  (
  `id` int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
  `createTime` datetime(0) NULL DEFAULT NULL COMMENT '创建时间',
  `lastmodifiedTime` datetime(0) NULL DEFAULT NULL COMMENT '修改时间',
  `channelCode` varchar(255) NOT NULL DEFAULT ''  COMMENT '机构编码',
  `channelName` varchar(255) NOT NULL DEFAULT ''  COMMENT '机构名称',
  `sortIndex` tinyint UNSIGNED NOT NULL DEFAULT 0  COMMENT '排序下标',
  `parentId` int(0) UNSIGNED NOT NULL DEFAULT 0  COMMENT '父机构编号',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT = '机构表' ROW_FORMAT = Dynamic;

-- -----------------------------
-- Table structure for sys_user
-- -----------------------------
DROP TABLE IF EXISTS `sys_user`;
CREATE TABLE `sys_user`  (
  `id` int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
  `createTime` datetime(0) NULL DEFAULT NULL COMMENT '创建时间',
  `lastmodifiedTime` datetime(0) NULL DEFAULT NULL COMMENT '修改时间',
  `channelId` int(0) UNSIGNED  NOT NULL DEFAULT 1   COMMENT '渠道Id',
  `deleted` tinyint  NOT NULL DEFAULT 0 COMMENT '逻辑删除 1=删除 0=未删除',
  `enabled` tinyint NOT NULL DEFAULT 1 COMMENT '用户状态 1=可用 0=删除',
  `name` varchar(255) NOT NULL DEFAULT '' COMMENT '用户昵称',
  `password` varchar(255)  NOT NULL DEFAULT '' COMMENT '密码',
  `roleId` int(0) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色Id',
  `salt` varchar(32) NOT NULL DEFAULT '' COMMENT '密码加密的盐',
  `username` varchar(64)  NOT NULL DEFAULT '' COMMENT '用户名',
  `email` varchar(36)  NOT NULL DEFAULT '' COMMENT '邮箱号',
  `mobile` varchar(18)  NOT NULL DEFAULT '' COMMENT '手机号',
  `googleSecret` varchar(36) NOT NULL DEFAULT '' COMMENT '谷歌验证器的秘钥',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE INDEX `UK51bvuyvihefoh4kp5syh2jpi4`(`username`) USING BTREE,
  INDEX `FKtlnkwkosadnhdypumotecrebo`(`channelId`) USING BTREE,
  INDEX `FK9s2sqg6p1req126agyn1sfeiy`(`roleId`) USING BTREE,
  CONSTRAINT `FK9s2sqg6p1req126agyn1sfeiy` FOREIGN KEY (`roleId`) REFERENCES `sys_role` (`id`) ON DELETE RESTRICT ON UPDATE RESTRICT,
  CONSTRAINT `FKtlnkwkosadnhdypumotecrebo` FOREIGN KEY (`channelId`) REFERENCES `sys_channel` (`id`) ON DELETE RESTRICT ON UPDATE RESTRICT
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT ='用户表'  ROW_FORMAT = Dynamic;

-- -----------------------------
-- Table structure for sys_menu
-- -----------------------------
DROP TABLE IF EXISTS `sys_menu`;
CREATE TABLE `sys_menu`  (
  `id` int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
  `createTime` datetime(0) NULL DEFAULT NULL COMMENT '创建时间',
  `lastmodifiedTime` datetime(0) NULL DEFAULT NULL COMMENT '修改时间',
  `icon` varchar(255)  NOT NULL DEFAULT '' COMMENT '菜单图标样式',
  `importStr` varchar(255) NOT NULL DEFAULT '' COMMENT '组件路径',
  `parentId` int(0) NOT NULL DEFAULT 0 COMMENT '父菜单编号',
  `path` varchar(255)  NOT NULL DEFAULT '' COMMENT '访问路径',
  `sortIndex` tinyint UNSIGNED NOT NULL DEFAULT 0 COMMENT '排序下标',
  `title` varchar(255)  NOT NULL DEFAULT '' COMMENT '菜单名称',
   `type` tinyint  NOT NULL DEFAULT 0 COMMENT '菜单类型  1=目录,2=菜单,3=权限',
  `outlink` tinyint  NOT NULL DEFAULT 0 COMMENT '是否外链 1=外链,0=内部菜单',
  `display` tinyint  NOT NULL DEFAULT 1 COMMENT '显示状态 1=显示,0=隐藏',
  `authorityStr` varchar(64)  NOT NULL DEFAULT 1 COMMENT '权限字符',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB  CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT ='菜单表' ROW_FORMAT = Dynamic;


-- ----------------------------------
-- Table structure for sys_role_menu
-- ----------------------------------
DROP TABLE IF EXISTS `sys_role_menu`;
CREATE TABLE `sys_role_menu`  (
  `menuId` int(0) UNSIGNED NOT NULL  COMMENT '菜单Id',
  `roleId` int(0) UNSIGNED NOT NULL  COMMENT '角色Id',
  `checked` int(0)  UNSIGNED NOT NULL DEFAULT 1 COMMENT '选中状态 1=选中 0=未选中',
  `createTime` datetime(0) NULL DEFAULT NULL,
  PRIMARY KEY (`menuId`, `roleId`) USING BTREE,
  FOREIGN KEY (`menuId`) REFERENCES `sys_menu` (`id`)
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT ='角色菜单表' ROW_FORMAT = Dynamic;

-- ---------------------------------
-- Table structure for sys_role_menu
-- ----------------------------------
DROP TABLE IF EXISTS `sys_operationlog`;
CREATE TABLE `sys_operationlog`  (
  `id` int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
  `createTime` datetime(0) NULL DEFAULT NULL  COMMENT '创建时间',
  `content` text  NULL  COMMENT '详细信息',
  `description` varchar(255) NOT NULL DEFAULT '' COMMENT '操作类型',
  `ipAddr` varchar(255)  NOT NULL DEFAULT '' COMMENT 'ip地址',
  `module` varchar(255)  NOT NULL DEFAULT '' COMMENT '模块',
  `username` varchar(255)  NOT NULL DEFAULT '' COMMENT '操作员',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB  CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT ='操作日志表' ROW_FORMAT = Dynamic;


-- ----------------------------------
-- Table structure for sys_loginlog
-- ----------------------------------
DROP TABLE IF EXISTS `sys_loginlog`;
CREATE TABLE `sys_loginlog`  (
  `id` int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
  `createTime` datetime(0) NULL DEFAULT NULL  COMMENT '创建时间',
  `ipAddr` varchar(255)  NULL DEFAULT NULL COMMENT '登录Ip地址',
  `username` varchar(255) NOT NULL DEFAULT '' COMMENT '用户名',
  `userType` varchar(255)  NOT NULL DEFAULT 1 COMMENT '用户类型 1=系统用户 2=app用户',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT ='操作日志表' ROW_FORMAT = Dynamic;

-- ----------------------------------
-- Table structure for sys_errorlog
-- ----------------------------------
DROP TABLE IF EXISTS `sys_errorlog`;
CREATE TABLE `sys_errorlog`  (
  `id` int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
  `createTime` datetime(0) NULL DEFAULT NULL COMMENT '创建时间',
  `errorMsg` varchar(255)  NOT NULL DEFAULT '' COMMENT '错误信息',
  `errorType` varchar(255)  NOT NULL DEFAULT '' COMMENT '错误类型',
  `ipAddr` varchar(255)  NOT NULL DEFAULT '' COMMENT '机器ip地址',
  `position` text   NULL COMMENT'异常发生的位置',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB  CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT ='错误日志表' ROW_FORMAT = Dynamic;

-- ----------------------------------
-- Table structure for sys_dictionary
-- ----------------------------------
DROP TABLE IF EXISTS `sys_dictionary`;
CREATE TABLE `sys_dictionary`  (
  `id` int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
  `createTime` datetime(0) NULL DEFAULT NULL COMMENT '创建时间',
  `lastmodifiedTime` datetime(0) NULL DEFAULT NULL COMMENT '修改时间',
  `dicName` varchar(64)  NOT NULL DEFAULT '' COMMENT '字典名称',
  `dicKey` varchar(32)  NOT NULL DEFAULT '' COMMENT '字典的key',
  `dicValue` varchar(255)  NOT NULL DEFAULT '' COMMENT '字典的value',
  `remark` varchar(64)  NOT NULL DEFAULT '' COMMENT '备注',
  UNIQUE INDEX `UK_dicKey`(`dicKey`) USING BTREE,
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT ='字典表' ROW_FORMAT = Dynamic;

-- ----------------------------------
-- Table structure for sys_message
-- ----------------------------------
DROP TABLE IF EXISTS `sys_message`;
CREATE TABLE `sys_message`  (
  `id` int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
  `createTime` datetime(0) NULL DEFAULT NULL COMMENT '创建时间',
  `lastmodifiedTime` datetime(0) NULL DEFAULT NULL COMMENT '修改时间',
  `keyName` varchar(255)  NOT NULL DEFAULT '' COMMENT '字典的key',
  `zhCn` text   NULL  COMMENT '中文',
  `zhTw` text   NULL  COMMENT '繁体中文',
   `enUs` text  NULL  COMMENT '英文',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT ='国际化信息表' ROW_FORMAT = Dynamic;

INSERT INTO `sys_dictionary` VALUES (1, '2020-11-03 10:46:11', '2023-04-07 09:59:13', '登录日志保存天数', 'loginLogSaveDay', '180', '-1永久保存');
INSERT INTO `sys_dictionary` VALUES (2, '2020-11-03 10:46:11', '2023-04-07 09:59:13', '操作日志保存天数', 'operationLogSaveDay', '180', '-1永久保存');
INSERT INTO `sys_dictionary` VALUES (3, '2020-11-03 10:46:11', '2023-04-07 09:59:13', '错误日志保存天数', 'errorLogSaveDay', '60', '-1永久保存');
INSERT INTO `sys_dictionary` VALUES (4, '2020-11-03 10:46:11', '2023-04-07 09:59:13', '单点登录', 'enableSso', 'false', 'true=单账号登录 false=允许多账号登录');
INSERT INTO `sys_dictionary` VALUES (5, '2023-01-09 17:13:00', '2023-04-07 09:59:13', '提现自动审核金额门槛', 'checkMinAmount', '500', '大于n需要后台管理员审核');
INSERT INTO `sys_dictionary` VALUES (6, '2023-01-09 17:13:00', '2023-04-07 09:59:13', '提现任务自动审核开关', 'enabledAutoCheck', 'true', '设置为true时候金额小于等于审核金额门槛的自动审核');
INSERT INTO `sys_dictionary` VALUES (7, '2023-01-09 17:13:00', '2023-04-07 09:59:13', 'tiktok任务自动审核开关', 'enabledTiktokAutoCheck', 'true', '设置为true开启tiktok任务的自动审核');
INSERT INTO `sys_dictionary` VALUES (8, '2023-01-09 17:13:00', '2023-04-07 09:59:13', 'tiktok自动审核延迟时间', 'tiktokAutoCheckLazyTime', '1', 'tiktok自动审核延迟时间 单位为分钟,用户提交审核后的指定分钟后自动审核');
INSERT INTO `sys_dictionary` VALUES (9, '2023-01-09 17:13:00', '2023-04-07 09:59:13', '推荐地址', 'recommendedAddress', 'https://www.easytrade.solutions/', '官网地址');
INSERT INTO `sys_dictionary` VALUES (10, '2023-01-09 17:13:00', '2023-04-07 09:59:13', '是否支持法币', 'supportFiat', 'false', 'true=支持 false=不支持');
INSERT INTO `sys_dictionary` VALUES (11, '2023-04-03 15:16:51', '2023-04-07 09:59:13', '初始密码', 'initPassword', '123456', '创建用户和重置密码的初始密码');
INSERT INTO `sys_dictionary` VALUES (12, '2023-04-03 15:16:51', '2023-04-10 10:29:39', 'ios下载地址', 'ios', 'http://47.242.107.138:9090/virtual/app/a_v1.0.1.apk', '');
INSERT INTO `sys_dictionary` VALUES (13, '2023-04-03 15:16:51', '2023-04-10 10:29:39', '安卓下载地址', 'android', 'http://47.242.107.138:9090/virtual/app/a_v1.0.1.apk', '');

INSERT INTO sys_channel (id, createTime, lastmodifiedTime, channelCode, channelName, sortIndex, parentId) VALUES (1, '2021-05-18 18:51:56', '2021-05-18 18:51:56', '1000', '总渠道', 1, 0);

INSERT INTO sys_role (id, createTime, lastmodifiedTime, roleCode, roleName) VALUES (1, '2019-05-14 16:26:12', '2019-05-14 16:26:12', 'user', '普通用户');
INSERT INTO sys_role (id, createTime, lastmodifiedTime, roleCode, roleName) VALUES (2, '2019-05-14 16:26:12', '2019-05-14 16:26:12', 'manager', '渠道负责人');
INSERT INTO sys_role (id, createTime, lastmodifiedTime, roleCode, roleName) VALUES (3, '2019-05-14 16:26:12', '2019-05-14 16:26:12', 'admin', '管理员');
INSERT INTO sys_role (id, createTime, lastmodifiedTime, roleCode, roleName) VALUES (5, '2023-05-04 15:46:50', '2023-05-04 15:46:50', 'proxy', '代理商');
-- ----------------------------
-- Records of sys_menu
-- ----------------------------
INSERT INTO `sys_menu` VALUES (2, '2021-05-14 16:26:00', '2023-04-06 12:27:50', 'PersonAddOutline', 'System/index', 0, '/sys/system', 7, '权限管理', 1, 0, 1, '-');
INSERT INTO `sys_menu` VALUES (3, '2021-05-14 16:26:00', '2023-04-06 12:08:05', 'PeopleOutline', 'System/pages/user/index', 2, '/sys/user', 1, '后台用户', 2, 0, 1, 'sys:user:list');
INSERT INTO `sys_menu` VALUES (4, '2021-05-14 16:26:00', '2023-04-06 12:28:07', 'ShirtOutline', 'System/pages/role/index', 2, '/sys/role', 2, '角色管理', 2, 0, 1, 'sys:role:list');
INSERT INTO `sys_menu` VALUES (6, '2021-05-14 16:26:00', '2023-04-06 12:05:07', 'DocumentTextOutline', 'Log/index', 0, '/sys/log', 8, '日志管理', 1, 0, 1, '-');
INSERT INTO `sys_menu` VALUES (7, '2021-05-13 17:20:00', '2023-04-06 11:07:56', 'EnterOutline', 'Log/pages/login/index', 6, '/log/login', 1, '登录日志', 2, 0, 1, 'sys:loginlog:list');
INSERT INTO `sys_menu` VALUES (8, '2021-05-13 17:20:00', '2023-04-06 11:08:14', 'BuildOutline', 'Log/pages/operation/index', 6, '/log/operation', 2, '操作日志', 2, 0, 1, 'sys:operationlog:list');
INSERT INTO `sys_menu` VALUES (13, '2021-05-14 16:26:00', '2023-04-06 11:54:30', 'MenuSharp', 'System/pages/menu/index', 2, '/sys/menu', 4, '菜单管理', 2, 0, 1, 'sys:menu:list');
INSERT INTO `sys_menu` VALUES (15, '2021-05-13 17:20:00', '2023-04-06 11:08:27', 'WarningOutline', 'Log/pages/error/index', 6, '/log/error', 3, '错误日志', 2, 0, 1, 'sys:errorlog:list');
INSERT INTO `sys_menu` VALUES (40, '2021-05-14 16:26:00', '2021-09-10 12:01:28', '', '', 3, '', 1, '新增', 3, 0, 1, 'sys:user:add');
INSERT INTO `sys_menu` VALUES (41, '2021-05-14 16:26:00', '2021-09-10 12:01:28', '', '', 3, '', 2, '修改', 3, 0, 1, 'sys:user:edit');
INSERT INTO `sys_menu` VALUES (42, '2021-05-14 16:26:00', '2021-09-10 12:01:28', '', '', 3, '', 3, '删除', 3, 0, 1, 'sys:user:remove');
INSERT INTO `sys_menu` VALUES (43, '2021-05-14 16:26:00', '2023-04-06 11:38:53', '', '', 3, '', 5, '导出', 3, 0, 1, 'sys:user:export');
INSERT INTO `sys_menu` VALUES (44, '2021-05-14 16:26:00', '2021-09-10 12:01:28', '', '', 4, '', 1, '新增', 3, 0, 1, 'sys:role:add');
INSERT INTO `sys_menu` VALUES (45, '2021-05-14 16:26:00', '2021-09-10 12:01:28', '', '', 4, '', 2, '修改', 3, 0, 1, 'sys:role:edit');
INSERT INTO `sys_menu` VALUES (46, '2021-05-14 16:26:00', '2021-09-10 12:01:28', '', '', 4, '', 3, '删除', 3, 0, 1, 'sys:role:remove');
INSERT INTO `sys_menu` VALUES (47, '2021-05-14 16:26:00', '2021-09-10 12:01:28', '', '', 4, '', 4, '导出', 3, 0, 1, 'sys:role:export');
INSERT INTO `sys_menu` VALUES (48, '2021-05-14 16:26:00', '2021-09-10 12:01:28', '', '', 13, '', 1, '新增', 3, 0, 1, 'sys:menu:add');
INSERT INTO `sys_menu` VALUES (49, '2021-05-14 16:26:00', '2021-09-10 12:01:28', '', '', 13, '', 2, '修改', 3, 0, 1, 'sys:menu:edit');
INSERT INTO `sys_menu` VALUES (50, '2021-05-14 16:26:00', '2021-09-10 12:01:28', '', '', 13, '', 3, '删除', 3, 0, 1, 'sys:menu:remove');
INSERT INTO `sys_menu` VALUES (52, '2021-05-14 16:26:00', '2023-04-06 11:40:50', '', '', 7, '', 1, '删除', 3, 0, 1, 'sys:loginlog:remove');
INSERT INTO `sys_menu` VALUES (53, '2021-05-14 16:26:00', '2023-04-06 11:41:15', '', '', 7, '', 3, '导出', 3, 0, 1, 'sys:loginlog:export');
INSERT INTO `sys_menu` VALUES (55, '2021-05-14 16:26:00', '2021-09-10 12:01:28', '', '', 8, '', 4, '导出', 3, 0, 1, 'sys:operationlog:export');
INSERT INTO `sys_menu` VALUES (56, '2021-05-14 16:26:00', '2023-04-06 11:41:51', '', '', 15, '', 2, '删除', 3, 0, 1, 'sys:errorlog:remove');
INSERT INTO `sys_menu` VALUES (57, '2021-05-14 16:26:00', '2023-04-06 11:41:54', '', '', 15, '', 3, '导出', 3, 0, 1, 'sys:errorlog:export');
INSERT INTO `sys_menu` VALUES (62, '2021-05-14 16:26:00', '2021-09-10 12:01:28', '', '', 3, '', 4, '重置密码', 3, 0, 1, 'sys:user:resetPwd');
INSERT INTO `sys_menu` VALUES (95, '2022-12-23 11:42:23', '2023-04-06 12:06:55', 'OptionsOutline', 'Control/index', 0, '/sys/control', 9, '监控管理', 1, 0, 1, '-');
INSERT INTO `sys_menu` VALUES (96, '2022-12-23 11:46:18', '2023-04-06 11:55:48', 'CloudyOutline', 'Control/pages/cache/index', 95, '/sys/control/cache', 1, '缓存监控', 2, 0, 1, 'sys:cache:list');
INSERT INTO `sys_menu` VALUES (97, '2022-12-23 11:46:18', '2023-04-06 11:55:58', 'ReaderOutline', 'Control/pages/cacheList/index', 95, '/sys/control/cacheList', 2, '缓存列表', 2, 0, 1, 'sys:cache:list');
INSERT INTO `sys_menu` VALUES (99, '2022-12-23 11:46:18', '2023-04-06 12:04:25', 'GitCompareOutline', 'Control/pages/sql/index', 95, '/47.242.107.138:9090/druid', 3, 'Sql监控', 2, 1, 1, 'sys:control:sql');
INSERT INTO `sys_menu` VALUES (116, '2021-05-14 16:26:00', '2023-04-06 12:13:50', 'BuildOutline', 'Settings/index', 0, '/settings', 6, '系统设置', 1, 0, 1, '-');
INSERT INTO `sys_menu` VALUES (117, '2023-04-06 10:51:19', '2023-04-10 10:14:07', 'SettingsOutline', 'Settings/pages/personal/index', 116, '/settings/personal', 1, '个人设置', 2, 0, 1, 'settings:personal:list');
INSERT INTO `sys_menu` VALUES (118, '2023-04-06 10:52:47', '2023-04-06 12:21:55', 'EllipsisHorizontalCircle', 'Settings/pages/system/index', 116, '/settings/system', 2, '系统设置', 2, 0, 1, 'settings:system:list');
INSERT INTO `sys_menu` VALUES (119, '2023-04-06 10:53:25', '2023-04-06 12:21:16', 'Analytics', 'Settings/pages/rate/index', 116, '/settings/rate', 3, '汇率管理', 2, 0, 1, 'settings:rate:list');
INSERT INTO `sys_menu` VALUES (120, '2023-04-06 10:54:04', '2023-04-06 12:13:42', 'FunnelOutline', 'Examine/index', 0, '/examine', 5, '审核管理', 1, 0, 1, '-');
INSERT INTO `sys_menu` VALUES (121, '2021-05-14 16:26:00', '2023-04-06 12:12:44', 'CreateOutline', 'Publish/index', 0, '/publish', 4, '发布管理', 1, 0, 1, '-');
INSERT INTO `sys_menu` VALUES (122, '2021-05-14 16:26:00', '2023-04-06 12:12:14', 'BarChartOutline', 'Data/index', 0, '/data', 3, '数据管理', 1, 0, 1, '-');
INSERT INTO `sys_menu` VALUES (123, '2021-05-14 16:26:00', '2023-04-06 12:19:56', 'ColorPaletteOutline', 'Portrait/index', 0, '/portrait', 2, '用户画像', 2, 0, 1, 'portrait:list');
INSERT INTO `sys_menu` VALUES (124, '2021-05-14 16:26:00', '2023-04-06 12:09:32', 'PeopleOutline', 'User/index', 0, '/user', 1, '用户管理', 2, 0, 1, 'user:list');
INSERT INTO `sys_menu` VALUES (125, '2023-04-06 11:03:12', '2023-04-06 12:31:09', 'PieChartOutline', 'Data/pages/user/index', 122, '/data/user', 1, '用户数据', 2, 0, 1, 'data:user:list');
INSERT INTO `sys_menu` VALUES (126, '2023-04-06 11:03:33', '2023-04-06 12:31:03', 'LogoUsd', 'Data/pages/fund/index', 122, '/data/fund', 2, '资金数据', 2, 0, 1, 'data:fund:list');
INSERT INTO `sys_menu` VALUES (127, '2023-04-06 11:04:00', '2023-04-06 12:22:34', 'ImagesOutline', 'Publish/pages/banner/index', 121, '/publish/banner', 1, 'Banner管理', 2, 0, 1, 'publish:banner:list');
INSERT INTO `sys_menu` VALUES (128, '2023-04-06 11:04:25', '2023-04-06 12:25:41', 'NewspaperOutline', 'Publish/pages/article/index', 121, '/publish/article', 2, '文章管理', 2, 0, 1, 'publish:article:list');
INSERT INTO `sys_menu` VALUES (129, '2023-04-06 11:04:44', '2023-04-06 12:26:27', 'NotificationsOutline', 'Publish/pages/notice/index', 121, '/publish/notice', 3, '公告管理', 2, 0, 1, 'publish:notice:list');
INSERT INTO `sys_menu` VALUES (130, '2023-04-06 11:05:09', '2023-04-06 12:25:17', 'MailOutline', 'Publish/pages/tiktok/index', 121, '/publish/tiktok', 4, 'Tiktok任务管理', 2, 0, 1, 'publish:tiktok:list');
INSERT INTO `sys_menu` VALUES (131, '2023-04-06 11:05:48', '2023-04-06 12:15:18', 'RestaurantOutline', 'Examine/pages/dinner/index', 120, '/examine/dinner', 1, '请客吃饭', 2, 0, 1, 'examine:dinner:list');
INSERT INTO `sys_menu` VALUES (132, '2023-04-06 11:06:07', '2023-04-06 12:16:45', 'CardOutline', 'Examine/pages/taking/index', 120, '/examine/taking', 2, '提现管理', 2, 0, 1, 'examine:taking:list');
INSERT INTO `sys_menu` VALUES (133, '2023-04-06 11:31:47', '2023-04-06 11:45:17', '', '', 126, '', 1, '关键指标', 3, 0, 1, 'data:fund:target');
INSERT INTO `sys_menu` VALUES (134, '2023-04-06 11:32:33', '2023-04-06 11:47:35', '', '', 126, '', 2, '出入金管理', 3, 0, 1, 'data:fund:access');
INSERT INTO `sys_menu` VALUES (135, '2023-04-06 11:32:54', '2023-04-06 11:47:40', '', '', 126, '', 3, '动静态管理', 3, 0, 1, 'data:fund:status');
INSERT INTO `sys_menu` VALUES (136, '2023-04-06 11:33:10', '2023-04-06 11:47:44', '', '', 126, '', 4, '余额持有管理', 3, 0, 1, 'data:fund:hold');
INSERT INTO `sys_menu` VALUES (137, '2023-04-06 11:33:59', '2023-04-06 11:48:34', '', '', 127, '', 1, '新增', 3, 0, 1, 'publish:banner:add');
INSERT INTO `sys_menu` VALUES (138, '2023-04-06 11:34:14', '2023-04-06 11:48:41', '', '', 127, '', 2, '编辑', 3, 0, 1, 'publish:banner:edit');
INSERT INTO `sys_menu` VALUES (139, '2023-04-06 11:34:22', '2023-04-06 11:48:51', '', '', 127, '', 3, '删除', 3, 0, 1, 'publish:banner:remove');
INSERT INTO `sys_menu` VALUES (140, '2023-04-06 11:34:38', '2023-04-06 11:49:05', '', '', 128, '', 1, '新增', 3, 0, 1, 'publish:article:add');
INSERT INTO `sys_menu` VALUES (141, '2023-04-06 11:34:57', '2023-04-06 11:49:25', '', '', 128, '', 2, '编辑', 3, 0, 1, 'publish:article:edit');
INSERT INTO `sys_menu` VALUES (142, '2023-04-06 11:35:05', '2023-04-06 11:49:31', '', '', 128, '', 3, '显示', 3, 0, 1, 'publish:article:show');
INSERT INTO `sys_menu` VALUES (143, '2023-04-06 11:35:12', '2023-04-06 11:49:46', '', '', 128, '', 4, '隐藏', 3, 0, 1, 'publish:article:hide');
INSERT INTO `sys_menu` VALUES (144, '2023-04-06 11:35:26', '2023-04-06 11:49:58', '', '', 128, '', 5, '删除', 3, 0, 1, 'publish:article:remove');
INSERT INTO `sys_menu` VALUES (145, '2023-04-06 11:35:35', '2023-04-06 11:50:28', '', '', 128, '', 6, '批量删除', 3, 0, 1, 'publish:article:batchremove');
INSERT INTO `sys_menu` VALUES (146, '2023-04-06 11:35:56', '2023-04-06 11:51:44', '', '', 129, '', 1, '新增', 3, 0, 1, 'publish:notice:add');
INSERT INTO `sys_menu` VALUES (147, '2023-04-06 11:36:04', '2023-04-06 11:51:51', '', '', 129, '', 2, '编辑', 3, 0, 1, 'publish:notice:edit');
INSERT INTO `sys_menu` VALUES (148, '2023-04-06 11:36:12', '2023-04-06 11:51:59', '', '', 129, '', 3, '删除', 3, 0, 1, 'publish:notice:remove');
INSERT INTO `sys_menu` VALUES (149, '2023-04-06 11:36:19', '2023-04-06 11:52:15', '', '', 129, '', 4, '批量删除', 3, 0, 1, 'publish:notice:batchremove');
INSERT INTO `sys_menu` VALUES (150, '2023-04-06 11:36:38', '2023-04-06 11:53:13', '', '', 130, '', 1, '新增', 3, 0, 1, 'publish:tiktok:add');
INSERT INTO `sys_menu` VALUES (151, '2023-04-06 11:36:48', '2023-04-06 11:53:20', '', '', 130, '', 2, '编辑', 3, 0, 1, 'publish:tiktok:edit');
INSERT INTO `sys_menu` VALUES (152, '2023-04-06 11:37:17', '2023-04-06 11:57:04', '', '', 4, '', 5, '批量删除', 3, 0, 1, 'sys:role:batchremove');
INSERT INTO `sys_menu` VALUES (153, '2023-04-06 11:38:48', '2023-04-06 11:56:47', '', '', 3, '', 6, '批量删除', 3, 0, 1, 'sys:user:batchremove');
INSERT INTO `sys_menu` VALUES (154, '2021-05-14 16:26:00', '2023-04-06 12:01:40', '', '', 131, '', 1, '通过', 3, 0, 1, 'examine:dinner:pass');
INSERT INTO `sys_menu` VALUES (155, '2021-05-14 16:26:00', '2023-04-06 12:01:56', '', '', 131, '', 2, '驳回', 3, 0, 1, 'examine:dinner:reject');
INSERT INTO `sys_menu` VALUES (156, '2021-05-14 16:26:00', '2023-04-06 12:01:08', '', '', 132, '', 1, '通过', 3, 0, 1, 'examine:taking:pass');
INSERT INTO `sys_menu` VALUES (157, '2021-05-14 16:26:00', '2023-04-06 12:01:26', '', '', 132, '', 1, '驳回', 3, 0, 1, 'examine:taking:reject');
INSERT INTO `sys_menu` VALUES (158, '2021-05-14 16:26:00', '2023-04-06 11:59:57', '', '', 7, '', 2, '批量删除', 3, 0, 1, 'sys:loginlog:batchremove');
INSERT INTO `sys_menu` VALUES (159, '2021-05-14 16:26:00', '2023-04-06 16:34:53', '', '', 15, '', 1, '批量删除', 3, 0, 1, 'sys:errorlog:batchremove');
INSERT INTO `sys_menu` VALUES (160, '2021-05-14 16:26:00', '2023-04-06 11:58:40', '', '', 117, '', 1, '更新个人信息', 3, 0, 1, 'settings:personal:update');
INSERT INTO `sys_menu` VALUES (161, '2021-05-14 16:26:00', '2023-04-06 11:58:54', '', '', 118, '', 1, '更新系统设置', 3, 0, 1, 'settings:system:update');
INSERT INTO `sys_menu` VALUES (162, '2021-05-14 16:26:00', '2023-04-06 11:59:22', '', '', 119, '', 1, '新增', 3, 0, 1, 'settings:rate:add');
INSERT INTO `sys_menu` VALUES (163, '2021-05-14 16:26:00', '2023-04-06 11:59:28', '', '', 119, '', 2, '编辑', 3, 0, 1, 'settings:rate:edit');
INSERT INTO `sys_menu` VALUES (164, '2021-05-14 16:26:00', '2023-04-06 11:44:18', '', '', 124, '', 1, '查看下级', 3, 0, 1, 'user:check:low');
INSERT INTO `sys_menu` VALUES (165, '2021-05-14 16:26:00', '2023-04-06 11:44:25', '', '', 124, '', 2, '查看信息', 3, 0, 1, 'user:check:info');
INSERT INTO `sys_menu` VALUES (166, '2023-04-06 14:31:19', '2023-04-06 14:31:19', '', '', 129, '', 1, '显示', 3, 0, 1, 'publish:notice:show');
INSERT INTO `sys_menu` VALUES (167, '2023-04-06 14:31:29', '2023-04-06 14:31:29', '', '', 129, '', 1, '隐藏', 3, 0, 1, 'publish:notice:hide');
INSERT INTO `sys_menu` VALUES (168, '2023-04-10 09:50:29', '2023-04-10 11:22:02', 'IdCardOutline', 'Examine/pages/tiktok/index', 120, '/examine/tiktok', 3, 'Tiktok任务审核', 2, 0, 1, 'examine:tiktok:list');
INSERT INTO `sys_menu` VALUES (169, '2023-04-10 09:50:29', '2023-04-10 09:51:43', '', '', 168, '', 1, '通过', 3, 0, 1, 'examine:tiktok:pass');
INSERT INTO `sys_menu` VALUES (170, '2023-04-10 09:50:29', '2023-04-10 09:51:56', '', '', 168, '', 1, '驳回', 3, 0, 1, 'examine:tiktok:reject');
INSERT INTO `sys_menu` VALUES (171, '2023-04-10 14:13:11', '2023-04-10 14:17:05', 'ServerOutline', 'Server/index', 0, '/sys/server', 10, '服务器信息', 1, 0, 1, 'sys:server:list');
INSERT INTO `sys_menu` VALUES (174, '2023-05-06 11:31:45', '2023-05-06 11:31:45', '', '', 124, '', 1, '添加', 3, 0, 1, 'user:add');
INSERT INTO `sys_menu` VALUES (175, '2023-05-06 11:58:19', '2023-05-06 11:58:24', '', '', 130, '', 3, '删除', 3, 0, 1, 'publish:tiktok:remove');
INSERT INTO `sys_menu` VALUES (176, '2023-05-06 11:58:19', '2023-05-06 11:59:06', '', '', 130, '', 4, '批量删除', 3, 0, 1, 'publish:tiktok:batchremove');
INSERT INTO `sys_menu` VALUES (177, '2023-05-10 14:20:10', '2023-05-10 15:35:36', 'VideocamOutline', 'Publish/pages/video/index', 121, '/publish/video', 5, '视频管理', 2, 0, 1, 'publish:video:list');
INSERT INTO `sys_menu` VALUES (178, '2023-05-10 15:36:56', '2023-05-10 15:36:56', '', '', 177, '', 1, '新增', 3, 0, 1, 'publish:video:add');
INSERT INTO `sys_menu` VALUES (179, '2023-05-10 15:37:33', '2023-05-10 15:37:33', '', '', 177, '', 2, '编辑', 3, 0, 1, 'publish:video:edit');
INSERT INTO `sys_menu` VALUES (180, '2023-05-10 15:37:49', '2023-05-10 15:37:49', '', '', 177, '', 3, '删除', 3, 0, 1, 'publish:video:remove');
INSERT INTO `sys_menu` VALUES (181, '2023-05-10 15:38:08', '2023-05-10 15:38:08', '', '', 177, '', 4, '批量删除', 3, 0, 1, 'publish:video:batchremove');
INSERT INTO `sys_menu` VALUES (182, '2023-05-10 15:38:56', '2023-05-10 15:38:56', '', '', 177, '', 5, '显示', 3, 0, 1, 'publish:video:show');
INSERT INTO `sys_menu` VALUES (183, '2023-05-10 15:39:12', '2023-05-10 15:39:12', '', '', 177, '', 6, '隐藏', 3, 0, 1, 'publish:video:hide');
INSERT INTO `sys_menu` VALUES (184, '2023-05-23 14:56:20', '2023-05-23 14:56:20', '', '', 124, '', 1, '设置代理商', 3, 0, 1, 'user:proxy');

INSERT INTO `sys_role_menu` VALUES (124, 5, 0, '2023-05-22 17:58:30');
INSERT INTO `sys_role_menu` VALUES (164, 5, 1, '2023-05-22 17:58:30');
INSERT INTO `sys_role_menu` VALUES (165, 5, 1, '2023-05-22 17:58:30');

INSERT INTO sys_user (id, createTime, lastmodifiedTime, channelId, deleted, enabled, name, password, roleId, salt, username,googleSecret) VALUES (1, null, null, 1, 0, 1, '管理员', 'E81C5B99F11123006C1F58DA7488281D', 3, '962012d09b8170d912f0669f6d7d933', 'admin','Z3ZSA6RFJ2WWJISX');