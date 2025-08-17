package com.mechanicalorchard.jclscan.model;

import lombok.Builder;
import lombok.Data;

/**
 * Relevant high-level details of an executable program.
 * 
 * This models a "load module" or "program object".
 * 
 * @see <a href="https://www.ibm.com/docs/en/zos/2.5.0?topic=introduction-zos-program-management-components">IBM Z/OS Program Management Components</a>
 */
@Data
@Builder
public final class ProgramSummary implements Program {
    private String fileName;
    private String programName;
    private Program.Kind kind;
    private int linesOfCode;
    private int numberOfConditionals;
    private int numberOfRoutines;

    @Override
    public String getName() {
        return programName;
    }
}


