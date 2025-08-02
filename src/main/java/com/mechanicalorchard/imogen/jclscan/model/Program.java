package com.mechanicalorchard.imogen.jclscan.model;

public sealed interface Program permits ProgRef {
    String getName();
}