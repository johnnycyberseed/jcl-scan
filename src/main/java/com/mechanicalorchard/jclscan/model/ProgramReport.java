package com.mechanicalorchard.jclscan.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ProgramReport {
    private final List<ProgramSummary> rows;

    public ProgramReport(List<ProgramSummary> rows) {
        this.rows = new ArrayList<>(rows);
    }

    public List<ProgramSummary> getRows() {
        return Collections.unmodifiableList(rows);
    }
}


