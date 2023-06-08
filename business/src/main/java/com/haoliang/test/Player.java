package com.haoliang.test;


import java.util.concurrent.CountDownLatch;

class Player implements Runnable{

    private CountDownLatch begin;

    private CountDownLatch end;

    Player(CountDownLatch begin,CountDownLatch end){
        this.begin = begin;
        this.end = end;
    }

    public void run() {

        try {
            begin.await();
            System.out.println(Thread.currentThread().getName() + " arrived !");;
            end.countDown();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

    }
}

