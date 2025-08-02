package com.mechanicalorchard.imogen.jclscan.model;

import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class JclJob {
    private String name;
    private List<JclStep> steps;
}