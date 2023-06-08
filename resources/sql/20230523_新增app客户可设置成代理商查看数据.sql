INSERT INTO `sys_menu` VALUES (184, '2023-05-23 14:56:20', '2023-05-23 14:56:20', '', '', 124, '', 1, '设置代理商', 3, 0, 1, 'user:proxy');

INSERT INTO `sys_role` VALUES (5, '2023-05-04 15:46:50', '2023-05-22 17:58:30', 'proxy', '代理商', 1);
INSERT INTO `sys_role_menu` VALUES (124, 5, 0, '2023-05-22 17:58:30');
INSERT INTO `sys_role_menu` VALUES (164, 5, 1, '2023-05-22 17:58:30');
INSERT INTO `sys_role_menu` VALUES (165, 5, 1, '2023-05-22 17:58:30');

alter table app_users Add column proxyRole tinyint(2) not null default 0;
