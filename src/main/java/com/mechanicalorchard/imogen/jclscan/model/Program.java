package com.mechanicalorchard.imogen.jclscan.model;

public sealed interface Program permits CobolFile, ProgRef {
    String getName();
}