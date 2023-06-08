package com.haoliang.test;

import javax.mail.*;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import java.util.Date;
import java.util.Properties;

public class Test {
    private static String from = "zbowen856@gmail.com";//发送者的谷歌邮箱

    public static void main(String[] args) {
        System.out.println(sendMailGMail("zbowen856@gmail.com", "123456"));
    }

    private static String password = "25083XxXx";//谷歌邮箱密码

    public static boolean sendMailGMail(String to, String content) {
        return gmailSender(from, password, to, "标题", content);
    }


    private static void gmailSsl(Properties props) {
        final String SSL_FACTORY = "javax.net.ssl.SSLSocketFactory";
        props.put("mail.debug", "true");
        props.put("mail.smtp.host", "smtp.gmail.com");
        props.put("mail.smtp.ssl.enable", "true");
        props.put("mail.smtp.socketFactory.class", SSL_FACTORY);
        props.put("mail.smtp.port", "587");
        //props.put("mail.smtp.port", "587");
        props.put("mail.smtp.socketFactory.port", "587");
        //props.put("mail.smtp.socketFactory.port", "587");
        props.put("mail.smtp.auth", "true");
    }


    public static boolean gmailSender(String from, String password, String email, String subject, String content) {

        Properties props = new Properties();
        gmailSsl(props);
        Session session = Session.getDefaultInstance(props,
                new Authenticator() {
                    protected PasswordAuthentication getPasswordAuthentication() {
                        return new PasswordAuthentication(from, password);
                    }
                });
        Message msg = new MimeMessage(session);

        try {
            msg.setFrom(new InternetAddress(from));
            msg.setRecipients(Message.RecipientType.TO,
                    InternetAddress.parse(email));
            msg.setSubject(subject);
            msg.setText(content);
            msg.setSentDate(new Date());
            Transport.send(msg);
        } catch (AddressException e) {
            e.printStackTrace();
        } catch (MessagingException e) {
            e.printStackTrace();
        }
        return true;
    }
}
