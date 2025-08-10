package com.mechanicalorchard.jclscan.model;

public class ProgramSummary {
    private final String fileName;
    private final String programName;
    private final String programType;
    private final int linesOfCode;
    private final int numberOfConditionals;
    private final int numberOfRoutines;

    public ProgramSummary(
            String fileName,
            String programName,
            String programType,
            int linesOfCode,
            int numberOfConditionals,
            int numberOfRoutines) {
        this.fileName = fileName;
        this.programName = programName;
        this.programType = programType;
        this.linesOfCode = linesOfCode;
        this.numberOfConditionals = numberOfConditionals;
        this.numberOfRoutines = numberOfRoutines;
    }

    public String getFileName() {
        return fileName;
    }

    public String getProgramName() {
        return programName;
    }

    public String getProgramType() {
        return programType;
    }

    public int getLinesOfCode() {
        return linesOfCode;
    }

    public int getNumberOfConditionals() {
        return numberOfConditionals;
    }

    public int getNumberOfRoutines() {
        return numberOfRoutines;
    }
}


