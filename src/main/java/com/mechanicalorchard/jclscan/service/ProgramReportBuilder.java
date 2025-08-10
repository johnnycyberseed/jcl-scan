package com.mechanicalorchard.jclscan.service;

import java.util.List;

import org.springframework.stereotype.Component;

import com.mechanicalorchard.jclscan.model.ProgramReport;
import com.mechanicalorchard.jclscan.model.ProgramSummary;

@Component
public class ProgramReportBuilder {

    public ProgramReport build(List<ProgramSummary> programSummaries) {
        return new ProgramReport(programSummaries);
    }
}


