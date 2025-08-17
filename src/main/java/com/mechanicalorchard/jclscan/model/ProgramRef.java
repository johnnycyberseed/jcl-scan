package com.mechanicalorchard.jclscan.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
/**
 * Represents the invocation of an executable program from a JCL job.
 */
public final class ProgramRef implements Program {
    private String name;
}