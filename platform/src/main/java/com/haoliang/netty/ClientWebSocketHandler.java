package com.haoliang.netty;


import com.alibaba.fastjson.JSONObject;
import com.haoliang.common.util.SpringUtil;
import com.haoliang.service.KLineDataService;
import io.netty.channel.Channel;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.SimpleChannelInboundHandler;
import io.netty.channel.group.ChannelGroup;
import io.netty.channel.group.DefaultChannelGroup;
import io.netty.handler.codec.http.FullHttpRequest;
import io.netty.handler.codec.http.websocketx.TextWebSocketFrame;
import io.netty.util.concurrent.GlobalEventExecutor;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Dominick Li
 * @createTime 2020/2/28  13:07
 * @description
 **/
@Slf4j
public class ClientWebSocketHandler extends SimpleChannelInboundHandler<TextWebSocketFrame> {

    /**
     * 存储已经登录用户的channel对象
     */
    private static ChannelGroup channelGroup = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE);

    KLineDataService kLineDataService = SpringUtil.getBean(KLineDataService.class);

    public static ChannelGroup getChannelGroup() {
        return channelGroup;
    }

    @Override
    public void channelActive(ChannelHandlerContext ctx) throws Exception {
        //添加到channelGroup通道组
        channelGroup.add(ctx.channel());
    }

    @Override
    public void channelInactive(ChannelHandlerContext ctx) throws Exception {
        //添加到channelGroup 通道组
        channelGroup.remove(ctx.channel());
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        //首次连接是FullHttpRequest，把用户id和对应的channel对象存储起来
        if (null != msg && msg instanceof FullHttpRequest) {
            channelGroup.add(ctx.channel());
        }
        String str = JSONObject.toJSONString(kLineDataService.getKLineNowData());
        ctx.channel().writeAndFlush(new TextWebSocketFrame(str));
        super.channelRead(ctx, msg);
    }

    @Override
    protected void channelRead0(ChannelHandlerContext channelHandlerContext, TextWebSocketFrame textWebSocketFrame) throws Exception {

    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
        super.exceptionCaught(ctx, cause);
        Channel channel = ctx.channel();
        //……
        if(channel.isActive())ctx.close();
    }

}
