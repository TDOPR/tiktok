package com.haoliang.model.vo;


public class DataVO {

    private String value;

    private String name;

    public DataVO() {
    }

    public DataVO(String name, String value) {
        this.name = name;
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
