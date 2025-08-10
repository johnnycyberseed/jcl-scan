package com.mechanicalorchard.jclscan.model;

public sealed interface Program permits CobolFile, EasytrieveFile, ProgramRef {
    String getName();
}