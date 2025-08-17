package com.mechanicalorchard.jclscan.model;

import lombok.Builder;
import lombok.Data;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Data
@Builder
public final class Procedure implements JclScript {
    private String name;
    @Builder.Default
    private Map<String, String> symbolicParameterDefaults = new HashMap<>();
    private List<JclStep> steps;
}