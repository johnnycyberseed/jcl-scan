package com.mechanicalorchard.jclscan.model;

import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public final class Procedure implements JclScript {
    private String name;
    private List<JclStep> steps;
}