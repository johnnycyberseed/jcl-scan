package com.mechanicalorchard.imogen.jclscan.model;

import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public final class JclFile implements JclProc {
    private String name;
    private List<JclStep> steps;
}