package com.mechanicalorchard.jclscan.model;

public sealed interface JclProc permits JclFile, ProcRef {
    String getName();
}