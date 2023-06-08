-- ----------------------------
-- Table structure for evm_token_address_pool
-- ----------------------------
DROP TABLE IF EXISTS `evm_token_address_pool`;
CREATE TABLE `evm_token_address_pool`
(
    `id`        bigint(18) NOT NULL AUTO_INCREMENT,
    `coin_id`   bigint(18) NOT NULL COMMENT '币种ID',
    `address`   varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '地址',
    `keystore`  varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT '' COMMENT 'keystore',
    `pwd`       varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT '' COMMENT '密码',
    `coin_type` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci  NOT NULL DEFAULT '' COMMENT '地址类型',
    `status`    int(10) NULL DEFAULT NULL,
    PRIMARY KEY (`id`) USING BTREE,
    INDEX       `unq_address` (`address`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_general_ci COMMENT = 'evm的地址池'
  ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for evm_token_recharge
-- ----------------------------
DROP TABLE IF EXISTS `evm_token_recharge`;
CREATE TABLE `evm_token_recharge`
(
    `id`               bigint(20) NOT NULL AUTO_INCREMENT,
    `uid`              bigint(18) NULL DEFAULT NULL COMMENT '用户UID',
    `coin_id`          bigint(18) NOT NULL DEFAULT 0 COMMENT '币种id',
    `coin_name`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '币种名称',
    `coin_type`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '币种类型',
    `address`          varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT '' COMMENT '钱包地址',
    `confirm`          int(1) NULL DEFAULT NULL COMMENT '充值确认数',
    `status`           int(4) NULL DEFAULT 0 COMMENT '状态：0-充值成功；1-到账成功，2-待支付',
    `txid`             varchar(80) CHARACTER SET utf8 COLLATE utf8_general_ci  NOT NULL DEFAULT '' COMMENT '交易id',
    `num`              decimal(20, 8) NULL DEFAULT NULL COMMENT '充值量',
    `fee`              decimal(20, 8) NULL DEFAULT NULL COMMENT '手续费',
    `mum`              decimal(20, 8) NULL DEFAULT NULL COMMENT '实际到账',
    `block_number`     int(10) NULL DEFAULT NULL COMMENT '交易的区块高度',
    `last_update_time` datetime(0) NULL DEFAULT NULL COMMENT '修改时间',
    `created`          datetime(0) NULL DEFAULT NULL COMMENT '创建时间',
    `type`             tinyint(1) NULL DEFAULT 0 COMMENT '充值',
    `remark`           varchar(64) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '备注',
    PRIMARY KEY (`id`) USING BTREE,
    UNIQUE INDEX `idx_txid` (`txid`) USING BTREE,
    INDEX              `uid_coinId_status` (`uid`, `coin_id`, `status`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8
  COLLATE = utf8_general_ci COMMENT = 'evm用户充值,当前用户充值成功之后添加数据到这个表,充值一般无手续费.当status为0和confirm=1的时候表示充值成功'
  ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for evm_token_withdraw
-- ----------------------------
DROP TABLE IF EXISTS `evm_token_withdraw`;
CREATE TABLE `evm_token_withdraw`
(
    `id`               bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `uid`              bigint(18) UNSIGNED NOT NULL COMMENT '用户id',
    `coin_id`          bigint(18) NOT NULL COMMENT '币种id',
    `coin_name`        varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '币种名称',
    `coin_type`        varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci  NOT NULL DEFAULT '' COMMENT '币种类型',
    `address`          varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '钱包地址',
    `txid`             varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '交易id',
    `num`              decimal(22, 8)                                                NOT NULL COMMENT '提现量(包含手续费)',
    `fee`              decimal(20, 8)                                                NOT NULL COMMENT '手续费',
    `mum`              decimal(22, 8)                                                NOT NULL COMMENT '实际提现量',
    `chain_fee`        decimal(20, 8) NULL DEFAULT NULL COMMENT '链上手续费花费',
    `block_num`        int(11) UNSIGNED NULL DEFAULT 0 COMMENT '区块高度',
    `remark`           varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '备注',
    `step`             tinyint(4) NULL DEFAULT NULL COMMENT '当前审核级数',
    `status`           tinyint(1) NOT NULL COMMENT '状态：0-审核中;1-成功;2-拒绝;3-撤销;4-审核通过;5-打币中;6;-待区块确认;7-区块打币失败',
    `audit_time`       datetime(0) NULL DEFAULT NULL COMMENT '审核时间',
    `last_update_time` datetime(0) NOT NULL COMMENT '修改时间',
    `create_time`      datetime(0) NOT NULL COMMENT '创建时间',
    `out_id`           bigint(18) NULL DEFAULT NULL,
    `monet_log_id`     int(11) NOT NULL DEFAULT 0 COMMENT '日志id',
    PRIMARY KEY (`id`) USING BTREE,
    INDEX              `uid` (`uid`) USING BTREE,
    INDEX              `idx_create_time` (`create_time`) USING BTREE,
    INDEX              `idx_status` (`status`) USING BTREE,
    INDEX              `coinid` (`coin_id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8mb4
  COLLATE = utf8mb4_general_ci COMMENT = '当用户发起提币的时候,把数据插入到该表'
  ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for evm_user_wallet
-- ----------------------------
DROP TABLE IF EXISTS `evm_user_wallet`;
CREATE TABLE `evm_user_wallet`
(
    `id`            bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'ID[钱包名]',
    `uid`           bigint(18) NOT NULL COMMENT '用户ID',
    `coin_id`       bigint(20) NOT NULL COMMENT '币种ID',
    `address`       varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci  NOT NULL COMMENT '钱包地址',
    `lower_address` varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci  NOT NULL COMMENT '小写地址',
    `password`      varchar(200) CHARACTER SET utf8 COLLATE utf8_general_ci  NOT NULL COMMENT '钱包密码',
    `keystore`      varchar(1024) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '秘钥文件',
    `valid`         char(1) CHARACTER SET utf8 COLLATE utf8_general_ci       NOT NULL COMMENT '是否可用：E可用，D不可用',
    `create_time`   datetime(0) NULL DEFAULT NULL COMMENT '创建时间',
    `update_time`   datetime(0) NULL DEFAULT NULL COMMENT '更新时间',
    `coin_type`     varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '网络类型名称',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB
  CHARACTER SET = utf8
  COLLATE = utf8_general_ci COMMENT = '用户数字货币钱包'
  ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for evm_token_coin_config
-- ----------------------------
DROP TABLE IF EXISTS `evm_token_coin_config`;
CREATE TABLE `evm_token_coin_config`
(
    `id`              bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'id',
    `coin`            varchar(20) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '币种',
    `coin_id`         bigint(20) NOT NULL COMMENT '币种ID',
    `coin_type`       varchar(32) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '币种类型',
    `contract`        varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '合约tokenID',
    `token`           varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '合约token名称',
    `round`           int(4) NOT NULL COMMENT '小数位数',
    `main_address`    varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '主钱包地址',
    `password`        varchar(200) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '主钱包密码',
    `collect_address` varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '归集钱包地址',
    `account_name`    varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '账户名',
    `block_no`        bigint(20) NOT NULL COMMENT '归集的区块高度',
    `txid`            varchar(64) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '归集的交易ID',
    `valid`           varchar(1) CHARACTER SET utf8 COLLATE utf8_general_ci  NOT NULL COMMENT '是否可用：E可用，D不可用',
    `fee`             decimal(20, 8) NULL DEFAULT 0.00000000 COMMENT '归集转账手续费',
    `remark`          varchar(50) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT '' COMMENT '备注',
    `create_time`     datetime NULL DEFAULT NULL COMMENT '创建时间',
    `update_time`     datetime NULL DEFAULT NULL COMMENT '更新时间',
    PRIMARY KEY (`id`) USING BTREE,
    UNIQUE INDEX `uidx_account_name_coin`(`account_name`, `coin`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 2 CHARACTER SET = utf8 COLLATE = utf8_general_ci COMMENT = 'eth token充值信息采集JOB表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for trx_token_address_pool
-- ----------------------------
DROP TABLE IF EXISTS `trx_token_address_pool`;
CREATE TABLE `trx_token_address_pool`
(
    `id`        bigint(18) NOT NULL AUTO_INCREMENT,
    `coin_id`   bigint(18) NOT NULL COMMENT '币种ID',
    `address`   varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '地址',
    `keystore`  varchar(1024) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT '' COMMENT 'keystore',
    `pwd`       varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT '' COMMENT '密码',
    `coin_type` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci  NOT NULL DEFAULT '' COMMENT '地址类型',
    PRIMARY KEY (`id`) USING BTREE,
    INDEX       `unq_address`(`address`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 613 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '用户trx的地址池' ROW_FORMAT = DYNAMIC;

-- ----------------------------
-- Table structure for trx_token_recharge
-- ----------------------------
DROP TABLE IF EXISTS `trx_token_recharge`;
CREATE TABLE `trx_token_recharge`
(
    `id`               bigint(20) NOT NULL AUTO_INCREMENT,
    `uid`              bigint(18) NULL DEFAULT NULL COMMENT '用户UID',
    `coin_id`          bigint(18) NOT NULL DEFAULT 0 COMMENT '币种id',
    `coin_name`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '币种名称',
    `coin_type`        varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '币种类型',
    `address`          varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT '' COMMENT '钱包地址',
    `confirm`          int(1) NULL DEFAULT NULL COMMENT '充值确认数',
    `status`           int(4) NULL DEFAULT 0 COMMENT '状态：0-待入帐；1-充值成功，2到账失败，3到账成功；',
    `txid`             varchar(80) CHARACTER SET utf8 COLLATE utf8_general_ci  NOT NULL DEFAULT '' COMMENT '交易id',
    `num`              decimal(20, 8) NULL DEFAULT NULL COMMENT '充值量',
    `fee`              decimal(20, 8) NULL DEFAULT NULL COMMENT '手续费',
    `mum`              decimal(20, 8) NULL DEFAULT NULL COMMENT '实际到账',
    `block_number`     int(10) NULL DEFAULT NULL COMMENT '交易的区块高度',
    `last_update_time` datetime NULL DEFAULT NULL COMMENT '修改时间',
    `created`          datetime NULL DEFAULT NULL COMMENT '创建时间',
    `type`             tinyint(1) NULL DEFAULT 0 COMMENT '充值',
    `remark`           varchar(64) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '备注',
    PRIMARY KEY (`id`) USING BTREE,
    UNIQUE INDEX `idx_txid`(`txid`) USING BTREE,
    INDEX              `uid_coinId_status`(`uid`, `coin_id`, `status`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 11 CHARACTER SET = utf8 COLLATE = utf8_general_ci COMMENT = '用户trx充值,当前用户充值成功之后添加数据到这个表,充值一般无手续费.当status为0和confirm=1的时候表示充值成功' ROW_FORMAT = DYNAMIC;

-- ----------------------------
-- Table structure for trx_token_withdraw
-- ----------------------------
DROP TABLE IF EXISTS `trx_token_withdraw`;
CREATE TABLE `trx_token_withdraw`
(
    `id`               bigint(18) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '自增id',
    `uid`              bigint(18) UNSIGNED NOT NULL COMMENT '用户id',
    `coin_id`          bigint(18) NOT NULL COMMENT '币种id',
    `coin_name`        varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '币种名称',
    `coin_type`        varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci  NOT NULL DEFAULT '' COMMENT '币种类型',
    `address`          varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '钱包地址',
    `txid`             varchar(128) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '交易id',
    `num`              decimal(22, 8)                                                NOT NULL COMMENT '提现量(包含手续费)',
    `fee`              decimal(20, 8)                                                NOT NULL COMMENT '手续费',
    `mum`              decimal(22, 8)                                                NOT NULL COMMENT '实际提现量',
    `chain_fee`        decimal(20, 8) NULL DEFAULT NULL COMMENT '链上手续费花费',
    `block_num`        int(11) UNSIGNED NULL DEFAULT 0 COMMENT '区块高度',
    `remark`           varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NULL DEFAULT NULL COMMENT '备注',
    `step`             tinyint(4) NULL DEFAULT NULL COMMENT '当前审核级数',
    `status`           tinyint(1) NOT NULL COMMENT '状态：0-审核中;1-成功;2-拒绝;3-撤销;4-审核通过;5-打币中;6;-待区块确认;7-区块打币失败',
    `audit_time`       datetime NULL DEFAULT NULL COMMENT '审核时间',
    `last_update_time` datetime                                                      NOT NULL COMMENT '修改时间',
    `created`          datetime                                                      NOT NULL COMMENT '创建时间',
    `out_id`           bigint(18) NULL DEFAULT NULL,
    `monet_log_id`     int(11) NULL DEFAULT 0 COMMENT '日志id',
    PRIMARY KEY (`id`) USING BTREE,
    INDEX              `uid`(`uid`) USING BTREE,
    INDEX              `idx_create_time`(`created`) USING BTREE,
    INDEX              `idx_status`(`status`) USING BTREE,
    INDEX              `coinid`(`coin_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 8 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '当用户发起TRX提币的时候,把数据插入到该表' ROW_FORMAT = DYNAMIC;


-- ----------------------------
-- Table structure for trx_user_wallet
-- ----------------------------
DROP TABLE IF EXISTS `trx_user_wallet`;
CREATE TABLE `trx_user_wallet`
(
    `id`            bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'ID[钱包名]',
    `uid`           bigint(18) NOT NULL COMMENT '用户ID',
    `coin_id`       bigint(20) NOT NULL COMMENT '币种ID',
    `address`       varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci  NOT NULL COMMENT '钱包地址',
    `lower_address` varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci  NOT NULL COMMENT '小写地址',
    `password`      varchar(200) CHARACTER SET utf8 COLLATE utf8_general_ci  NOT NULL COMMENT '钱包密码',
    `keystore`      varchar(1024) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '秘钥文件',
    `valid`         char(1) CHARACTER SET utf8 COLLATE utf8_general_ci       NOT NULL COMMENT '是否可用：E可用，D不可用',
    `create_time`   datetime NULL DEFAULT NULL COMMENT '创建时间',
    `update_time`   datetime NULL DEFAULT NULL COMMENT '更新时间',
    PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 3 CHARACTER SET = utf8 COLLATE = utf8_general_ci COMMENT = 'TRX 钱包' ROW_FORMAT = DYNAMIC;

-- ----------------------------
-- Table structure for trx_token_coin_config
-- ----------------------------
DROP TABLE IF EXISTS `trx_token_coin_config`;
CREATE TABLE `trx_token_coin_config`
(
    `id`              bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'id',
    `coin`            varchar(20) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '币种',
    `coin_id`         bigint(20) NOT NULL COMMENT '币种ID',
    `coin_type`       varchar(32) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL COMMENT '币种类型',
    `contract`        varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '合约tokenID',
    `token`           varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '合约token名称',
    `round`           int(4) NOT NULL COMMENT '小数位数',
    `main_address`    varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '主钱包地址',
    `password`        varchar(200) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '主钱包密码',
    `collect_address` varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '归集钱包地址',
    `account_name`    varchar(100) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '账户名',
    `block_no`        bigint(20) NOT NULL COMMENT '归集的区块高度',
    `txid`            varchar(64) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL COMMENT '归集的交易ID',
    `valid`           varchar(1) CHARACTER SET utf8 COLLATE utf8_general_ci  NOT NULL COMMENT '是否可用：E可用，D不可用',
    `fee`             decimal(20, 8) NULL DEFAULT 0.00000000 COMMENT '归集转账手续费',
    `remark`          varchar(50) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT '' COMMENT '备注',
    `create_time`     datetime NULL DEFAULT NULL COMMENT '创建时间',
    `update_time`     datetime NULL DEFAULT NULL COMMENT '更新时间',
    PRIMARY KEY (`id`) USING BTREE,
    UNIQUE INDEX `uidx_account_name_coin`(`account_name`, `coin`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 6 CHARACTER SET = utf8 COLLATE = utf8_general_ci COMMENT = 'eth token充值信息采集JOB表' ROW_FORMAT = DYNAMIC;

-- ----------------------------
-- Records of trx_token_coin_config
-- ----------------------------