-- -------------------------------
-- Table structure for tree_paths
-- -------------------------------
DROP TABLE IF EXISTS `test`;
CREATE TABLE `test`  (
  `id` int(0) UNSIGNED NOT NULL AUTO_INCREMENT  COMMENT '父Id',
  `name` int(0) UNSIGNED NOT NULL DEFAULT  0 COMMENT '子Id',
  `password` int(0) UNSIGNED NOT NULL DEFAULT  0 COMMENT '子是父的第几代',
  `createTime` datetime(0) NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `lastmodifiedTime` datetime(0) NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB  CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT = '测试表' ROW_FORMAT = Dynamic;
