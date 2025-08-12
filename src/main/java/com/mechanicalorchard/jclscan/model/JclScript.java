package com.mechanicalorchard.jclscan.model;

public sealed interface JclScript permits Job, Procedure, ProcedureRef {
    String getName();
}