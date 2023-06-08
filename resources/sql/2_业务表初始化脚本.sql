-- --------------------------------
-- Table structure for appUsers 用户表
-- -------------------------------
DROP TABLE IF EXISTS `app_users`;
CREATE TABLE `app_users`
(
    `id`               int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `email`            varchar(64)  NOT NULL DEFAULT '' COMMENT '邮箱账号',
    `password`         varchar(56)  NOT NULL DEFAULT '' COMMENT '密码',
    `salt`             varchar(32)  NOT NULL DEFAULT '' COMMENT '密码加密的盐',
    `mobile`           varchar(32)  NOT NULL DEFAULT '' COMMENT '手机号',
    `inviteCode`       varchar(20)  NOT NULL DEFAULT '' COMMENT '邀请码',
    `headImage`        varchar(255) NOT NULL DEFAULT '' COMMENT '头像',
    `nickName`         varchar(255) NOT NULL DEFAULT '' COMMENT '用户昵称',
    `enabled`          tinyint(2) UNSIGNED NOT NULL DEFAULT 1 COMMENT '用户状态： 1-正常 0=禁用',
    `inviteId`         int(0) UNSIGNED NOT NULL DEFAULT 0 COMMENT '邀请人Id',
    `loginCount`       int(0) UNSIGNED NOT NULL DEFAULT 0 COMMENT '登录次数',
    `level`            tinyint(2) UNSIGNED NOT NULL DEFAULT 0 COMMENT '社区等级',
    `vipLevel`         tinyint(2) UNSIGNED NOT NULL DEFAULT 0 COMMENT 'Vip等级',
    `valid`            tinyint(2) UNSIGNED NOT NULL DEFAULT 0 COMMENT '有效用户标识 1=有效 0=无效',
    `greenhorn`        tinyint(2) UNSIGNED NOT NULL DEFAULT 1 COMMENT '是否新手 1=新手 0=非新手',
    `proxyRole`        tinyint(2) UNSIGNED NOT NULL DEFAULT 0 COMMENT '是否为代理商 1=是',
    `createTime`       datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `lastmodifiedTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    UNIQUE INDEX `UK_app_users_email` (`email`) USING BTREE,
    UNIQUE INDEX `UK_app_users_inviteCode` (`inviteCode`) USING BTREE,
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT=10000
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = 'app用户表'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for tiktok_account tiktok账户关联表
-- -------------------------------
DROP TABLE IF EXISTS `tiktok_account`;
CREATE TABLE `tiktok_account`
(
    `id`         varchar(64) NOT NULL,
    `userId`     int(0) UNSIGNED NOT NULL COMMENT '用户Id',
    `username`   varchar(64) NOT NULL DEFAULT '' COMMENT 'tiktop用户名',
    `active`     tinyint(2) UNSIGNED NOT NULL DEFAULT 1 COMMENT '使用状态 1-使用 0=未使用',
    `createTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    PRIMARY KEY (`id`) USING BTREE,
    CONSTRAINT `FK_tiktok_account_userId` FOREIGN KEY (`userId`) REFERENCES `app_users` (`id`)
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = 'tiktop账户关联表'
  ROW_FORMAT = Dynamic;

-- ---------------------------
-- Table structure for wallets
-- ---------------------------
DROP TABLE IF EXISTS `wallets`;
CREATE TABLE `wallets`
(
    `id`                   bigint UNSIGNED NOT NULL,
    `userId`               int(0) UNSIGNED NOT NULL,
    `blockAddress`         varchar(255)   NOT NULL DEFAULT '' COMMENT '区块链地址',
    `legalCurrencyAccount` varchar(255)   NOT NULL DEFAULT '' COMMENT '提现用的法币账号',
    `usdWalletAmount`      decimal(12, 2) NOT NULL DEFAULT 0.00000000 COMMENT 'USD钱包余额',
    `walletAmount`         decimal(14, 2) NOT NULL DEFAULT 0.00000000 COMMENT 'TTT钱包余额',
    `frozenAmount`         decimal(12, 2) UNSIGNED NOT NULL DEFAULT 0.00000000 COMMENT '冻结的Usd金额',
    `totalTaskNum`         bigint UNSIGNED NOT NULL DEFAULT 0 COMMENT '发布任务的总次数',
    `hasTaskNum`           bigint UNSIGNED NOT NULL DEFAULT 0 COMMENT '可用次数',
    `createTime`           datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `lastmodifiedTime`     datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    PRIMARY KEY (`id`) USING BTREE,
    UNIQUE INDEX `wallets_userId` (`userId`) USING BTREE,
    CONSTRAINT `FK_wallet_userId` FOREIGN KEY (`userId`) REFERENCES `app_users` (`id`)
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '用户钱包表'
  ROW_FORMAT = Dynamic;

-- ---------------------------
-- Table structure for wallets_usd_withdraw
-- ---------------------------

DROP TABLE IF EXISTS `wallets_usd_withdraw`;
CREATE TABLE `wallets_usd_withdraw`
(
    `id`               bigint UNSIGNED NOT NULL,
    `txid`             varchar(255)   NOT NULL DEFAULT '' COMMENT '单号',
    `userId`           int(0) UNSIGNED NOT NULL COMMENT '用户Id',
    `coinId`           tinyint(2) UNSIGNED NOT NULL DEFAULT 1 COMMENT '提现渠道',
    `coinType`         varchar(255)   NOT NULL DEFAULT '' COMMENT '链的类型',
    `amount`           decimal(12, 2) NOT NULL DEFAULT 0.00 COMMENT '提现金额',
    `free`             decimal(10, 2) NOT NULL DEFAULT 0.00 COMMENT '手续费',
    `actualAmount`     decimal(12, 2) NOT NULL DEFAULT 0.00 COMMENT '实际提现金额',
    `address`          varchar(255)   NOT NULL DEFAULT '' COMMENT '提币地址',
    `status`           tinyint(2) UNSIGNED NOT NULL DEFAULT 0 COMMENT '0=待审核  2=拒绝  4=审核通过',
    `usdLogsId`        bigint UNSIGNED NULL COMMENT '提现流水的Id',
    `createTime`       datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `lastmodifiedTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    `auditTime`        datetime(0) NULL COMMENT '审核时间',
    PRIMARY KEY (`id`) USING BTREE,
    CONSTRAINT `FK_wallets_withdraw_userId` FOREIGN KEY (`userId`) REFERENCES `app_users` (`id`)
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = 'Usd提现表'
  ROW_FORMAT = Dynamic;

-- ---------------------------
-- Table structure for wallets_usd_recharge
-- ---------------------------

DROP TABLE IF EXISTS `wallets_usd_recharge`;
CREATE TABLE `wallets_usd_recharge`
(
    `id`               bigint UNSIGNED NOT NULL,
    `txid`             varchar(255)   NOT NULL DEFAULT '' COMMENT '单号',
    `userId`           int(0) UNSIGNED NOT NULL COMMENT '用户Id',
    `coinId`           tinyint(2) UNSIGNED NOT NULL DEFAULT 1 COMMENT '充值渠道',
    `coinType`         varchar(255)   NOT NULL DEFAULT '' COMMENT '链的类型',
    `amount`           decimal(12, 2) NOT NULL DEFAULT 0.00 COMMENT '充值金额',
    `free`             decimal(10, 2) NOT NULL DEFAULT 0.00 COMMENT '手续费',
    `actualAmount`     decimal(12, 2) NOT NULL DEFAULT 0.00 COMMENT '实际充值金额',
    `address`          varchar(255)   NOT NULL DEFAULT '' COMMENT '充币地址',
    `status`           tinyint(2) UNSIGNED NOT NULL DEFAULT 0 COMMENT '状态：0-待入账(法币充值为下单成功待支付)；1-充值成功，2到账失败，3到账成功',
    `createTime`       datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `lastmodifiedTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    PRIMARY KEY (`id`) USING BTREE,
    CONSTRAINT `FK_wallets_usd_recharge_userId` FOREIGN KEY (`userId`) REFERENCES `app_users` (`id`)
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = 'Usd充值表'
  ROW_FORMAT = Dynamic;


-- ---------------------------
-- Table structure for fiat_recharge_order
-- ---------------------------

DROP TABLE IF EXISTS `fiat_recharge_order`;
CREATE TABLE `fiat_recharge_order`
(
    `id`               bigint UNSIGNED NOT NULL,
    `userId`           int(0) UNSIGNED NOT NULL COMMENT '用户Id',
    `fiatType`         tinyint(2) UNSIGNED NOT NULL COMMENT '法币类型  1=印尼 2=越南 3=泰国',
    `platformOrderNo`  varchar(64)    NOT NULL DEFAULT '' COMMENT '平台订单',
    `url`              text           NOT NULL COMMENT '支付链接',
    `status`           tinyint(2) UNSIGNED NOT NULL DEFAULT 2 COMMENT '订单状态：1=未支付,2=支付成功,3=撤销,4=支付失败',
    `amount`           decimal(12, 2) NOT NULL DEFAULT 0.00 COMMENT '充值金额',
    `fee`              decimal(10, 2) NOT NULL DEFAULT 0.00 COMMENT '手续费',
    `actualAmount`     decimal(12, 2) NOT NULL DEFAULT 0.00 COMMENT '实际充值金额',
    `exchangeRate`     decimal(12, 2) NOT NULL DEFAULT 0.00 COMMENT 'usd换法币的汇率',
    `usdAmount`        decimal(10, 2) NOT NULL DEFAULT 0.00 COMMENT 'usd金额',
    `createTime`       datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `lastmodifiedTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    PRIMARY KEY (`id`) USING BTREE,
    CONSTRAINT `FK_fiat_recharge_order_userId` FOREIGN KEY (`userId`) REFERENCES `app_users` (`id`)
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '法币充值表'
  ROW_FORMAT = Dynamic;

DROP TABLE IF EXISTS `fiat_withdraw_order`;
CREATE TABLE `fiat_withdraw_order`
(
    `id`               bigint UNSIGNED NOT NULL,
    `userId`           int(0) UNSIGNED NOT NULL COMMENT '用户Id',
    `fiatType`         tinyint(2) UNSIGNED NOT NULL COMMENT '法币类型  1=印尼 2=越南 3=泰国',
    `platformOrderNo`  varchar(64)    NOT NULL DEFAULT '' COMMENT '平台订单',
    `status`           tinyint(2) NOT NULL DEFAULT 2 COMMENT '订单状态： 0=待处理 1=已受理 2=提现成功 4=提现失败,5=银行代付中, 8=待审核  9=审核通过,延迟T+2发放 10=调用代付接口异常',
    `bankCode`         varchar(255)   NOT NULL DEFAULT '' COMMENT '提现银行编码',
    `bankNumber`       varchar(255)   NOT NULL DEFAULT '' COMMENT '提现银行卡号',
    `name`             varchar(255)   NOT NULL DEFAULT '' COMMENT '持卡姓名',
    `amount`           decimal(12, 2) NOT NULL DEFAULT 0.00 COMMENT '提现法币金额',
    `fee`              decimal(10, 2) NOT NULL DEFAULT 0.00 COMMENT '手续费',
    `actualAmount`     decimal(12, 2) NOT NULL DEFAULT 0.00 COMMENT '实际提现金额',
    `exchangeRate`     decimal(12, 2) NOT NULL DEFAULT 0.00 COMMENT 'usd换法币的汇率',
    `usdAmount`        int            NOT NULL DEFAULT 1 COMMENT '提现的usd金额',
    `createTime`       datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `lastmodifiedTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    PRIMARY KEY (`id`) USING BTREE,
    CONSTRAINT `FK_fiat_withdraw_order_userId` FOREIGN KEY (`userId`) REFERENCES `app_users` (`id`)
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '法币提现表'
  ROW_FORMAT = Dynamic;

-- -------------------------------
-- Table structure for wallet_logs
-- -------------------------------
DROP TABLE IF EXISTS `wallet_usd_logs`;
CREATE TABLE `wallet_usd_logs`
(
    `id`         bigint UNSIGNED NOT NULL,
    `userId`     int(0) UNSIGNED NOT NULL DEFAULT 0 COMMENT '用户ID',
    `amount`     decimal(12, 2) UNSIGNED NOT NULL COMMENT '本次变动金额',
    `action`     tinyint(2) UNSIGNED NOT NULL COMMENT '收支动作:1-收入 0-支出',
    `type`       tinyint(2) UNSIGNED NOT NULL COMMENT '流水类型 1-充值  2-提现  3-购买VIP等级 4-购买任务次数包套餐',
    `status`     tinyint(2) UNSIGNED NOT NULL DEFAULT 0 COMMENT '提现状态 0=提现成功 1=审核中 2=驳回 3=驳回撤销流水',
    `coinId`     tinyint(2) UNSIGNED NOT NULL DEFAULT 0 COMMENT '充值和提现渠道 0=USDT 1=印尼盾 2=越南盾 3=泰铢',
    `createTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
     PRIMARY KEY (`id`) USING BTREE,
    INDEX        `type` (`type`) USING BTREE,
    CONSTRAINT `FK_wallet_usd_logs_userId` FOREIGN KEY (`userId`) REFERENCES `app_users` (`id`)
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '钱包usd账号日志表'
  ROW_FORMAT = Dynamic;

-- -------------------------------
-- Table structure for wallet_logs
-- -------------------------------
DROP TABLE IF EXISTS `wallet_ttt_logs`;
CREATE TABLE `wallet_ttt_logs`
(
    `id`         bigint UNSIGNED NOT NULL,
    `userId`     int(0) UNSIGNED NOT NULL DEFAULT 0 COMMENT '用户ID',
    `amount`     decimal(14, 2) UNSIGNED NOT NULL COMMENT '本次变动金额',
    `action`     tinyint(2) UNSIGNED NOT NULL COMMENT '收支动作:1-收入 2-支出',
    `type`       tinyint(2) UNSIGNED NOT NULL COMMENT '流水类型 1-代数奖  2-团队奖 3-分红奖 4=持币奖  5=点赞 6=关注 7=评论 8=转出到USD账号 9=注册福利 10=抵扣购买VIP',
    `createTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `zero`       tinyint(2) UNSIGNED NOT NULL DEFAULT 0 COMMENT '零撸用户产生 1=是 0=否',
    PRIMARY KEY (`id`) USING BTREE,
    INDEX        `type` (`type`) USING BTREE,
    CONSTRAINT `FK_wallet_ttt_logs_userId` FOREIGN KEY (`userId`) REFERENCES `app_users` (`id`)
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '钱包ttt账号日志表'
  ROW_FORMAT = Dynamic;

-- -------------------------------
-- Table structure for freeze_proxy_logs
-- -------------------------------
DROP TABLE IF EXISTS `freeze_proxy_logs`;
CREATE TABLE `freeze_proxy_logs`
(
    `id`         int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `amount`     decimal(12, 2) UNSIGNED NOT NULL COMMENT '收益',
    `userId`     int(0) UNSIGNED NOT NULL COMMENT '用户Id',
    `type`       tinyint(2) UNSIGNED NOT NULL COMMENT '流水类型1-代数奖 2-团队奖 3-分红奖',
    `createTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    PRIMARY KEY (`id`) USING BTREE,
    INDEX        `type` (`type`) USING BTREE,
    CONSTRAINT `FK_freeze_proxy_logs_userId` FOREIGN KEY (`userId`) REFERENCES `app_users` (`id`)
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '7天待领取冻结收益'
  ROW_FORMAT = Dynamic;

-- ------------------------------
-- Table structure for tree_paths
-- -------------------------------
DROP TABLE IF EXISTS `tree_paths`;
CREATE TABLE `tree_paths`
(
    `ancestor`   int(0) UNSIGNED NOT NULL DEFAULT 0 COMMENT '父Id',
    `descendant` int(0) UNSIGNED NOT NULL DEFAULT 0 COMMENT '子Id',
    `level`      int(0) UNSIGNED NOT NULL DEFAULT 0 COMMENT '子是父的第几代',
    PRIMARY KEY (`ancestor`, `descendant`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '用户邀请关系表'
  ROW_FORMAT = Dynamic;

-- ----------------------------------
-- Table structure for  update_user_level_job
-- ----------------------------------
DROP TABLE IF EXISTS `update_user_level_job`;
CREATE TABLE `update_user_level_job`
(
    `userId`    int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `delayTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '任务延迟处理时间',
    PRIMARY KEY (`userId`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT ='更新用户的代理商等级'
  ROW_FORMAT = Dynamic;


-- ----------------------------------
-- Table structure for business_job
-- ---------------------------------
DROP TABLE IF EXISTS `business_job`;
CREATE TABLE `business_job`
(
    `createDate`      datetime(0) NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建日期',
    `holdingCoinTask` tinyint(2) NOT NULL DEFAULT 0 COMMENT '发放持币奖任务是否执行 1=已执行 0=未执行',
    `algebraTask`     tinyint(2) NOT NULL DEFAULT 0 COMMENT '发放代数奖任务是否执行 1=已执行 0=未执行',
    `teamTask`        tinyint(2) NOT NULL DEFAULT 0 COMMENT '发放团队奖任务是否执行 1=已执行 0=未执行',
    `specialTask`     tinyint(2) NOT NULL DEFAULT 0 COMMENT '发放分红奖任务是否执行 1=已执行 0=未执行',
    UNIQUE INDEX `UK_job_createDate` (`createDate`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT ='收益发放任务表'
  ROW_FORMAT = Dynamic;


-- --------------------------------
-- Table structure for banner 轮播图
-- -------------------------------
DROP TABLE IF EXISTS `banner`;
CREATE TABLE `banner`
(
    `id`               bigint UNSIGNED NOT NULL,
    `name`             varchar(64) NOT NULL DEFAULT '' COMMENT '图片名称',
    `zhPath`           varchar(64) NOT NULL DEFAULT '' COMMENT '中文路径',
    `enPath`           varchar(64) NOT NULL DEFAULT '' COMMENT '英文路径',
    `inPath`           varchar(64) NOT NULL DEFAULT '' COMMENT '印尼语路径',
    `thPath`           varchar(64) NOT NULL DEFAULT '' COMMENT '泰语路径',
    `viPath`           varchar(64) NOT NULL DEFAULT '' COMMENT '越南语路径',
    `sortIndex`        tinyint UNSIGNED NOT NULL DEFAULT 0 COMMENT '排序',
    `enabled`          tinyint UNSIGNED NOT NULL DEFAULT 1 COMMENT '显示状态 1=显示 0=隐藏',
    `createTime`       datetime(0) NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `lastmodifiedTime` datetime(0) NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '首页轮播图'
  ROW_FORMAT = Dynamic;

INSERT INTO `banner`
VALUES (1645991862114533376, '1', '/home/tiktok/data/banner/1645991862114533376/zh_CN.jpg',
        '/home/tiktok/data/banner/1645991862114533376/en_US.jpg',
        '/home/tiktok/data/banner/1645991862114533376/in_ID.jpg',
        '/home/tiktok/data/banner/1645991862114533376/th_TH.jpg',
        '/home/tiktok/data/banner/1645991862114533376/vi_VN.jpg', 1, 1, '2023-04-12 11:26:56', '2023-04-12 11:26:56');
INSERT INTO `banner`
VALUES (1645992052707901440, '2', '/home/tiktok/data/banner/1645992052707901440/zh_CN.jpg',
        '/home/tiktok/data/banner/1645992052707901440/en_US.jpg',
        '/home/tiktok/data/banner/1645992052707901440/in_ID.jpg',
        '/home/tiktok/data/banner/1645992052707901440/th_TH.jpg',
        '/home/tiktok/data/banner/1645992052707901440/vi_VN.jpg', 2, 1, '2023-04-12 11:27:41', '2023-04-12 11:28:24');
INSERT INTO `banner`
VALUES (1645992173575159808, '3', '/home/tiktok/data/banner/1645992173575159808/zh_CN.jpg',
        '/home/tiktok/data/banner/1645992173575159808/en_US.jpg',
        '/home/tiktok/data/banner/1645992173575159808/in_ID.jpg',
        '/home/tiktok/data/banner/1645992173575159808/th_TH.jpg',
        '/home/tiktok/data/banner/1645992173575159808/vi_VN.jpg', 3, 1, '2023-04-12 11:28:10', '2023-04-12 11:28:20');


-- --------------------------------
-- Table structure for vip_orders vip套餐订单记录
-- -------------------------------
DROP TABLE IF EXISTS `vip_orders`;
CREATE TABLE `vip_orders`
(
    `id`               int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `userId`           int(0) UNSIGNED NOT NULL COMMENT '购买的用户Id',
    `level`            tinyint(2) UNSIGNED NOT NULL COMMENT 'vip等级',
    `total`            decimal(7, 2) NOT NULL COMMENT '收益上限总量',
    `allowance`        decimal(7, 2) NOT NULL DEFAULT 0.00 COMMENT '剩余收益余量',
    `frozenAmount`     decimal(7, 2) NOT NULL DEFAULT 0.00 COMMENT '已接取任务冻结的金额',
    `valid`            tinyint(2) UNSIGNED NOT NULL DEFAULT 1 COMMENT '是否有效 1=有效 0=无效',
    `createTime`       datetime(0) NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `lastmodifiedTime` datetime(0) NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    PRIMARY KEY (`id`) USING BTREE,
    CONSTRAINT `FK_vip_orders_userId` FOREIGN KEY (`userId`) REFERENCES `app_users` (`id`)
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = 'vip套餐订单记录'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for tiktok_task 关注和点赞任务
-- -------------------------------
DROP TABLE IF EXISTS `tiktok_task`;
CREATE TABLE `tiktok_task`
(
    `id`               bigint UNSIGNED NOT NULL,
    `userId`           int(0) UNSIGNED NULL COMMENT '发布任务的B端用户id ',
    `username`         varchar(64)  NOT NULL DEFAULT '' COMMENT 'tiktok用户名',
    `opusId`           text NULL COMMENT '作品Id',
    `num`              int(0) UNSIGNED NOT NULL COMMENT '任务的数量',
    `hasNum`           int(0) UNSIGNED NOT NULL COMMENT '剩余的数量',
    `tiktokUserId`     varchar(255) NOT NULL DEFAULT '' COMMENT 'tiktok用户Id',
    `type`             tinyint(2) UNSIGNED NOT NULL COMMENT '任务类型 1=关注 2=点赞',
    `built`            tinyint(2) UNSIGNED NOT NULL COMMENT '是否系统内置任务 0=非内置 1=Telegram任务 2=Facebook任务 3=Twitter任务',
    `createTime`       datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `lastmodifiedTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    PRIMARY KEY (`id`) USING BTREE,
    CONSTRAINT `FK_tiktok_task_userId` FOREIGN KEY (`userId`) REFERENCES `app_users` (`id`)
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = 'tiktok关注和点赞任务'
  ROW_FORMAT = Dynamic;

INSERT INTO `tiktok_task`
VALUES (1645243981360451588, NULL, 'Official Twitter', 'https://twitter.com/Tiktokguild', 100000, 100000, '', 1, 3,
        '2023-04-10 09:55:07', '2023-04-10 09:55:07');
INSERT INTO `tiktok_task`
VALUES (1645243981360451589, NULL, 'Official Facebook', 'https://www.facebook.com/profile.php?id=100091423895551',
        100000, 100000, '', 1, 2, '2023-04-10 09:55:07', '2023-04-10 09:55:07');
INSERT INTO `tiktok_task`
VALUES (1645243981360451590, NULL, 'Official Telegram', 'https://t.me/Tiktokguild', 100000, 100000, '', 1, 1,
        '2023-04-10 09:55:07', '2023-04-10 09:55:07');

-- --------------------------------
-- Table structure for tiktok_task_prices 任务发布套餐价格
-- -------------------------------
DROP TABLE IF EXISTS `tiktok_task_prices`;
CREATE TABLE `tiktok_task_prices`
(
    `id`               int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `num`              int(0) UNSIGNED NOT NULL COMMENT '任务数量',
    `price`            int(0) UNSIGNED NOT NULL COMMENT '套餐价格',
    `visible`          tinyint(2) UNSIGNED NOT NULL DEFAULT 1 COMMENT '页面是否显示该套餐 1=显示 0=隐藏',
    `createTime`       datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `lastmodifiedTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '任务发布套餐价格'
  ROW_FORMAT = Dynamic;

INSERT INTO `tiktok_task_prices`
VALUES (1, 1000, 699, 1, '2023-02-27 14:57:36', '2023-02-27 14:57:36'),
       (2, 3000, 1980, 1, '2023-02-27 14:57:36', '2023-02-27 14:57:36'),
       (3, 10000, 6800, 1, '2023-02-27 14:57:36', '2023-02-27 14:57:36');

-- --------------------------------
-- Table structure for task 套餐购买订单表
-- -------------------------------
DROP TABLE IF EXISTS `tiktok_task_price_orders`;
CREATE TABLE `tiktok_task_price_orders`
(
    `id`         int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `priceId`    int(0) UNSIGNED NOT NULL COMMENT '购买的套餐Id',
    `userId`     int(0) UNSIGNED NOT NULL COMMENT '用户Id',
    `createTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    PRIMARY KEY (`id`) USING BTREE,
    CONSTRAINT `FK_tiktok_task_price_orders_userId` FOREIGN KEY (`userId`) REFERENCES `app_users` (`id`)
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '购买套餐订单表'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for task 客户度用户已接取的任务
-- -------------------------------
DROP TABLE IF EXISTS `app_user_task`;
CREATE TABLE `app_user_task`
(
    `id`               bigint UNSIGNED NOT NULL,
    `userId`           int(0) UNSIGNED NOT NULL COMMENT '发布任务的B端用户id',
    `taskId`           bigint UNSIGNED NOT NULL COMMENT 'toktik任务Id',
    `vipOrderIds`      text NULL COMMENT '对应使用的vip套餐Id列表json字符',
    `status`           tinyint(2) UNSIGNED NOT NULL DEFAULT 1 COMMENT '任务状态  1=已接取  2=待审核 3=驳回  4=已完成',
    `imgUrl`           varchar(128)  NOT NULL DEFAULT '' COMMENT '待审需要的截图文件存储路径',
    `amount`           decimal(8, 2) NOT NULL DEFAULT 0.00 COMMENT '任务奖励',
    `level`            tinyint(2) NOT NULL DEFAULT 0 COMMENT '接单套餐的vip等级',
    `createTime`       datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `lastmodifiedTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    `auditTime`        datetime(0) NULL COMMENT '审核时间',
    UNIQUE (userId, taskId),
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = 'tiktok关注和点赞任务'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for kline_data K线数据
-- -------------------------------
DROP TABLE IF EXISTS `kline_data`;
CREATE TABLE `kline_data`
(
    `id`         int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `createDate` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建日期',
    `kopen`      decimal(4, 2) NOT NULL DEFAULT 0.00 COMMENT '当天开盘价',
    `khigh`      decimal(4, 2) NOT NULL DEFAULT 0.00 COMMENT '当天最高价',
    `klow`       decimal(4, 2) NOT NULL DEFAULT 0.00 COMMENT '当日最低价',
    `kclose`     decimal(4, 2) NOT NULL DEFAULT 0.00 COMMENT '当日收盘价',
    `kvol`       decimal(4, 2) NOT NULL DEFAULT 0.00 COMMENT '当日成交量',
    PRIMARY KEY (`id`) USING BTREE,
    UNIQUE INDEX `UK_kline_data_createDate` (`createDate`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = 'K线数据'
  ROW_FORMAT = Dynamic;


-- ----------------------------------
-- Table structure for app_versions
-- ----------------------------------
DROP TABLE IF EXISTS `app_versions`;
CREATE TABLE `app_versions`
(
    `id`              int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `createTime`      datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `systemName`      varchar(255) NOT NULL DEFAULT '' COMMENT '系统名称 ios 、android',
    `version`         varchar(255) NOT NULL DEFAULT '' COMMENT '版本号',
    `znUpdateDesc`    varchar(255) NOT NULL DEFAULT '' COMMENT '中文 功能更新说明',
    `enUpdateDesc`    varchar(255) NOT NULL DEFAULT '' COMMENT '英语 功能更新说明',
    `inUpdateDesc`    varchar(255) NOT NULL DEFAULT '' COMMENT '印度尼西亚文 功能更新说明',
    `thUpdateDesc`    varchar(255) NOT NULL DEFAULT '' COMMENT '泰语 功能更新说明',
    `viUpdateDesc`    varchar(255) NOT NULL DEFAULT '' COMMENT '越南语 功能更新说明',
    `downloadAddress` varchar(255) NOT NULL DEFAULT '' COMMENT 'app下载地址',
    `active`          tinyint(2) NOT NULL DEFAULT 0 COMMENT '最新版本标识 1=是 0=否',
    `forceUpdate`     tinyint(2) NOT NULL DEFAULT 0 COMMENT '强制更新标识 1=是 0=否',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT ='系统版本表'
  ROW_FORMAT = Dynamic;

INSERT INTO `app_versions`
VALUES (1, '2023-03-13 18:49:31', 'ios', 'v1.0.0', '首个版本', '英文描述', '印度尼西亚语', '泰语', '越南语', 'https://www.baidu.com/', 1,
        0),
       (2, '2023-03-13 18:49:31', 'android', 'v1.0.0', '首个版本', '英文描述', '印度尼西亚语', '泰语', '越南语',
        'http://47.242.107.138:9091/virtual/app/android/et_101.apk#/login', 1, 0);

-- -------------------------------
-- Table structure for treat_guest_dinner
-- -------------------------------
DROP TABLE IF EXISTS `treat_guest_dinner`;
CREATE TABLE `treat_guest_dinner`
(
    `id`               int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `masterUserId`     int(0) UNSIGNED NOT NULL COMMENT '请客的主人用户ID',
    `itemUserIds`      text COMMENT '新用户ID',
    `videoUrl`         varchar(255) NOT NULL DEFAULT '' COMMENT '视频播放地址',
    `imgUrl`           varchar(255) NOT NULL DEFAULT '' COMMENT '封面图地址',
    `address`          varchar(64)  NOT NULL DEFAULT '' COMMENT '报销地址',
    `coinType`         varchar(8) NULL DEFAULT '' COMMENT '链的类型',
    `status`           tinyint(2) NOT NULL DEFAULT 2 COMMENT '任务状态 2=待审核 3=驳回  4=已完成',
    `auditTime`        datetime(0) NULL COMMENT '审核时间',
    `createTime`       datetime(0) NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
    `lastmodifiedTime` datetime(0) NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    CONSTRAINT `FK_treat_guest_dinner_userId` FOREIGN KEY (`masterUserId`) REFERENCES `app_users` (`id`),
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '请客吃饭管理'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for stat_user
-- -------------------------------
DROP TABLE IF EXISTS `stat_user`;
CREATE TABLE `stat_user`
(
    `id`           int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `createDate`   datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建日期',
    `total`        int(0) UNSIGNED NOT NULL DEFAULT 0 COMMENT '总用户',
    `valid`        int(0) UNSIGNED NOT NULL DEFAULT 0 COMMENT '有效用户',
    `zero`         int(0) UNSIGNED NOT NULL DEFAULT 0 COMMENT '零撸用户',
    `yesterdayAdd` int(0) UNSIGNED NOT NULL DEFAULT 0 COMMENT '昨日新增',
    `community`    int(0) UNSIGNED NOT NULL DEFAULT 0 COMMENT '社区用户',
    PRIMARY KEY (`id`) USING BTREE,
    UNIQUE INDEX `UK_kline_data_createDate` (`createDate`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = ' 统计用户数量信息'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for stat_input_and_output
-- -------------------------------
DROP TABLE IF EXISTS `stat_input_and_output`;
CREATE TABLE `stat_input_and_output`
(
    `id`         int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `createDate` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建日期',
    `input`      decimal(20, 2) NOT NULL DEFAULT 0.00 COMMENT '入金',
    `output`     decimal(20, 2) NOT NULL DEFAULT 0.00 COMMENT '出金',
    `tvlTtt`     decimal(20, 2) NOT NULL DEFAULT 0.00 COMMENT '当日TVL 含TTT',
    `tvl`        decimal(20, 2) NOT NULL DEFAULT 0.00 COMMENT '当日TVL 不含TTT',
    `bubble`     decimal(20, 2) NOT NULL DEFAULT 0.00 COMMENT '泡沫 tvl-tvlTtt',
    PRIMARY KEY (`id`) USING BTREE,
    UNIQUE INDEX `UK_stat_input_and_output_createDate` (`createDate`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '统计出入金信息'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for stat_dynamic_and_static
-- -------------------------------
DROP TABLE IF EXISTS `stat_dynamic_and_static`;
CREATE TABLE `stat_dynamic_and_static`
(
    `id`         int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `createDate` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建日期',
    `dynamic`    decimal(16, 2) NOT NULL DEFAULT 0.00 COMMENT '当天发放的总动态',
    `statics`    decimal(16, 2) NOT NULL DEFAULT 0.00 COMMENT '当天发放的总静态',
    PRIMARY KEY (`id`) USING BTREE,
    UNIQUE INDEX `UK_stat_dynamic_and_static_createDate` (`createDate`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '统计动静态管理'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for stat_balance
-- -------------------------------
DROP TABLE IF EXISTS `stat_balance`;
CREATE TABLE `stat_balance`
(
    `id`         int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `createDate` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建日期',
    `usd`        decimal(16, 2) NOT NULL DEFAULT 0.00 COMMENT 'USD持有',
    `ttt`        decimal(18, 2) NOT NULL DEFAULT 0.00 COMMENT 'TTT持有',
    `tttToUsd`   decimal(16, 2) NOT NULL DEFAULT 0.00 COMMENT 'TTT转USD持有',
    PRIMARY KEY (`id`) USING BTREE,
    UNIQUE INDEX `UK_stat_balance_createDate` (`createDate`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '余额持有管理'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for article 文章
-- -------------------------------
DROP TABLE IF EXISTS `article`;
CREATE TABLE `article`
(
    `id`               int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `createTime`       datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建日期',
    `lastmodifiedTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    `path`             varchar(64)  NOT NULL DEFAULT '' COMMENT '文件存储途径',
    `bannerUrl`        varchar(255) NOT NULL DEFAULT '' COMMENT '文章背景图',
    `enabled`          tinyint(2) UNSIGNED NOT NULL DEFAULT 1 COMMENT '启用状态： 1-启用 0=禁用',
    `title`            varchar(255) NOT NULL DEFAULT '' COMMENT '标题',
    `description`      varchar(255) NOT NULL DEFAULT '' COMMENT '简介',
    `language`         tinyint(3) NOT NULL DEFAULT 1 COMMENT '语种 1=中文 2=英文 3=印尼语 4=泰语 5=越南语',
    `info`             mediumtext COMMENT '内容',
    `source`           varchar(64) NULL COMMENT '来源',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '文章'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for video 公会相关视频
-- -------------------------------
DROP TABLE IF EXISTS `video`;
CREATE TABLE `video`
(
    `id`               int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `createTime`       datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建日期',
    `lastmodifiedTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    `title`            varchar(255) NOT NULL DEFAULT '' COMMENT '标题',
    `enabled`          tinyint(2) UNSIGNED NOT NULL DEFAULT 1 COMMENT '启用状态： 1-启用 0=禁用',
    `language`         tinyint(3) NOT NULL DEFAULT 1 COMMENT '语种 1=中文 2=英文 3=印尼语 4=泰语 5=越南语',
    `folderPath`       varchar(64)  NOT NULL DEFAULT '' COMMENT '文件存储目录',
    `bannerUrl`        varchar(255) NOT NULL DEFAULT '' COMMENT '封面图路径',
    `playUrl`          varchar(255) NOT NULL DEFAULT '' COMMENT '播放地址',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '公会相关视频'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for news 公告
-- -------------------------------
DROP TABLE IF EXISTS `news`;
CREATE TABLE `news`
(
    `id`               int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `createTime`       datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建日期',
    `lastmodifiedTime` datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '修改时间',
    `path`             varchar(64)  NOT NULL DEFAULT '' COMMENT '新闻背景图存储途径',
    `bannerUrl`        varchar(255) NOT NULL DEFAULT '' COMMENT '新闻背景图',
    `enabled`          tinyint(2) UNSIGNED NOT NULL DEFAULT 1 COMMENT '启用状态： 1-启用 0=禁用',
    `type`             tinyint(2) UNSIGNED NOT NULL DEFAULT 1 COMMENT '类型： 1-文章 2=公告',
    `forceStatus`      tinyint(2) UNSIGNED NOT NULL DEFAULT 1 COMMENT '强制通知 1=是 0=否,只有一个强制通知存在',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '新闻'
  ROW_FORMAT = Dynamic;

-- ----------------------------------
-- Table structure for news_user
-- ----------------------------------
DROP TABLE IF EXISTS `news_user`;
CREATE TABLE `news_user`
(
    `newsId` int(0) UNSIGNED NOT NULL COMMENT '公告Id',
    `userId` int(0) UNSIGNED NOT NULL COMMENT '用户Id',
    PRIMARY KEY (`newsId`, `userId`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT ='公告和用户之间的引用关系' ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for news 公告_中文
-- -------------------------------
DROP TABLE IF EXISTS `news_zh_cn`;
CREATE TABLE `news_zh_cn`
(
    `id`          int(0) UNSIGNED NOT NULL,
    `title`       varchar(255) NOT NULL DEFAULT '' COMMENT '标题',
    `description` varchar(255) NOT NULL DEFAULT '' COMMENT '简介',
    `info`        mediumtext COMMENT '内容',
    `source`      varchar(64) NULL COMMENT '来源',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '公告_中文'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for news 公告_英文
-- -------------------------------
DROP TABLE IF EXISTS `news_en_us`;
CREATE TABLE `news_en_us`
(
    `id`          int(0) UNSIGNED NOT NULL,
    `title`       varchar(255) NOT NULL DEFAULT '' COMMENT '标题',
    `description` varchar(255) NOT NULL DEFAULT '' COMMENT '简介',
    `info`        mediumtext COMMENT '内容',
    `source`      varchar(64) NULL COMMENT '来源',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '公告_英文'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for news 公告_印度尼西亚
-- -------------------------------
DROP TABLE IF EXISTS `news_in_id`;
CREATE TABLE `news_in_id`
(
    `id`          int(0) UNSIGNED NOT NULL,
    `title`       varchar(255) NOT NULL DEFAULT '' COMMENT '标题',
    `description` varchar(255) NOT NULL DEFAULT '' COMMENT '简介',
    `info`        mediumtext COMMENT '内容',
    `source`      varchar(64) NULL COMMENT '来源',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '公告_印度尼西亚'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for news 公告_泰国
-- -------------------------------
DROP TABLE IF EXISTS `news_th_th`;
CREATE TABLE `news_th_th`
(
    `id`          int(0) UNSIGNED NOT NULL,
    `title`       varchar(255) NOT NULL DEFAULT '' COMMENT '标题',
    `description` varchar(255) NOT NULL DEFAULT '' COMMENT '简介',
    `info`        mediumtext COMMENT '内容',
    `source`      varchar(64) NULL COMMENT '来源',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '公告_泰国'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for news 公告_越南 国家
-- -------------------------------
DROP TABLE IF EXISTS `news_vi_vn`;
CREATE TABLE `news_vi_vn`
(
    `id`          int(0) UNSIGNED NOT NULL,
    `title`       varchar(255) NOT NULL DEFAULT '' COMMENT '标题',
    `description` varchar(255) NOT NULL DEFAULT '' COMMENT '简介',
    `info`        mediumtext COMMENT '内容',
    `source`      varchar(64) NULL COMMENT '来源',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '公告_越南'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for currency_exchange_rates
-- -------------------------------
DROP TABLE IF EXISTS `currency_exchange_rates`;
CREATE TABLE `currency_exchange_rates`
(
    `id`           int(0) UNSIGNED NOT NULL AUTO_INCREMENT,
    `updateTime`   datetime(0) NULL DEFAULT CURRENT_TIMESTAMP COMMENT '更新时间',
    `source`       varchar(8)     NOT NULL DEFAULT '' COMMENT '源货币',
    `target`       varchar(8)     NOT NULL DEFAULT '' COMMENT '目标货币',
    `exchangeRate` decimal(16, 3) NOT NULL DEFAULT 0.00 COMMENT '源货币转目标货币的汇率',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '法币汇率'
  ROW_FORMAT = Dynamic;

-- --------------------------------
-- Table structure for update_pwd_log 密码修改时间记录表
-- -------------------------------
DROP TABLE IF EXISTS `update_pwd_log`;
CREATE TABLE `update_pwd_log`
(
    `userId`           int(0) UNSIGNED NOT NULL,
    `lastmodifiedTime` datetime(0) COMMENT '最后一次修改时间',
     PRIMARY KEY (`userId`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_unicode_ci COMMENT = '密码修改时间记录表'
  ROW_FORMAT = Dynamic;

INSERT INTO `app_users`
VALUES (10000, 'tiktokguild@gmail.com', 'B080DC6D8A146AAF2458AFA8B3233903', 'ecc8f0dc893b4bbe9d194506c3a911bc', '',
        '888888', '', 'tiktokguild@gmail.com', 1, 0, 1, 0, 0, 0, 0, '2023-03-23 04:26:02', '2023-04-07 04:26:02');
INSERT INTO `wallets`
VALUES (1644194796069445633, 10000, '', '', 0.00, 0.00, 0.00, 0, 0, '2023-04-07 04:26:02', '2023-04-07 04:26:02');
INSERT INTO `tree_paths`
VALUES (10000, 10000, 0);
INSERT INTO `vip_orders`
VALUES (1, 10000, 0, 15.00, 15.00, 0.00, 1, '2023-04-07 04:26:02', '2023-04-07 04:26:02');
