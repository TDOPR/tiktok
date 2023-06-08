/*查看团队有效用户人数 app_users表的valid字段=1标识有效用户  0=无效用户 */
select count(*) from tree_paths t
left join app_users u on t.descendant=u.id
where  ancestor=10100 and t.level> 0  and u.valid=1

/*查看团队有效用户的总持币金额*/
SELECT sum(w.walletAmount) FROM tree_paths t
LEFT JOIN wallets w ON t.descendant = w.userId
LEFT JOIN app_users u ON u.id = t.descendant
WHERE t.ancestor = 10100 AND t.level>0 AND u.valid = 1

/*查看团队的三代用户的静态收益   wallet_ttt_logs表里的type= 5,6,7 的时候为静态收益 */
SELECT sum(p.amount) totalAmount,t.level FROM tree_paths t
LEFT JOIN wallet_ttt_logs p ON t.descendant=p.userId
LEFT JOIN app_users u ON u.id=t.descendant
WHERE t.ancestor=10100 AND u.valid=1 AND p.zero=0
AND p.createTime BETWEEN DATE_FORMAT(DATE_SUB(CURDATE(), INTERVAL 1 DAY), '%Y-%m-%d 00:00:00')
AND DATE_FORMAT(DATE_SUB(CURDATE(), INTERVAL 1 DAY), '%Y-%m-%d 23:59:59')
AND t.level in (1,2,3)
AND p.type in (5,6,7)
group by t.level

/*查询可以分红的用户数*/
select count(*) from app_users where `level`>=4
/*查询整个社区有效用户的静态收益总和   用这个金额/分红用户数 再乘以对应的分红比例得出分红奖*/
select sum(amount) from wallet_ttt_logs w
left join app_users u on w.userId=u.id
where u.valid=1 and w.type in(5,6,7) AND w.zero=0
AND w.createTime BETWEEN DATE_FORMAT(DATE_SUB(CURDATE(), INTERVAL 1 DAY), '%Y-%m-%d 00:00:00')
AND DATE_FORMAT(DATE_SUB(CURDATE(), INTERVAL 1 DAY), '%Y-%m-%d 23:59:59')


/*查询昨天有效用户产生的静态收益*/
select sum(amount)amount,userId,level from wallet_ttt_logs w
left join app_users u on w.userId=u.id
where u.valid=1 and u.enabled=1 and type in (5,6,7)
AND w.createTime BETWEEN DATE_FORMAT(DATE_SUB(CURDATE(), INTERVAL 1 DAY), '%Y-%m-%d 00:00:00')
AND DATE_FORMAT(DATE_SUB(CURDATE(), INTERVAL 1 DAY), '%Y-%m-%d 23:59:59')
GROUP BY userId

/*查询该用户的社区等级大于0的上级用户信息*/
SELECT t.ancestor userId,u.level  as '社区等级',t.level as '家族等级' FROM tree_paths t
LEFT JOIN app_users u ON u.id = t.ancestor
WHERE t.descendant = 10101 AND t.level > 0  AND u.level > 0
ORDER BY t.level

/*查询指定用户的静态收益记录*/
select * from wallet_ttt_logs where userId =10100  and type in(5,6,7) order by createTime desc

/*查询指定用户的动态收益记录*/
select * from wallet_ttt_logs where userId =10100  and type in(1,2,3,4) order by createTime desc

/*删除指定用户的接单数据和新手标识*/
delete from app_user_task where userId=10002;
update app_users set greenhorn=1 where id=10002;


/*资金数据关键指标*/
select (select (select sum(amount) from wallet_usd_logs where type=1) - (select sum(amount) from wallet_usd_logs where type=2 and status=0)) as 'TVL',
        (select sum(amount) from wallet_usd_logs where type=1) as '历史总入金',
        (select sum(amount) from wallet_usd_logs where type=2 and status=0) as '历史总出金',
        (select sum(amount) from wallet_usd_logs where type=1  and  createTime>DATE_FORMAT(CURDATE(), '%Y-%m-%d 00:00:00')) as '今日入金' ,
        (select sum(amount) from wallet_usd_logs where type=2 and status=0  and  createTime>DATE_FORMAT(CURDATE(), '%Y-%m-%d 00:00:00')) as '今日出金';

/*用户数据关键指标*/
select (select count(*)  from app_users) as '总用户',
        (select count(*) from app_users where valid=1) as '有效用户用户',
        (select count(*) from app_users where valid=0) as '零撸用户',
        (select count(*) from app_users where valid=0) as '社区用户' ;

