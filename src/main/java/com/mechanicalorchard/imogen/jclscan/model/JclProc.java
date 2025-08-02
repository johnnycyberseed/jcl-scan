package com.mechanicalorchard.imogen.jclscan.model;

public sealed interface JclProc permits JclFile, ProcRef {
    String getName();
}