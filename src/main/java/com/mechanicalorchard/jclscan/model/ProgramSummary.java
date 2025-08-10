package com.mechanicalorchard.jclscan.model;

import lombok.Builder;
import lombok.Data;

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


