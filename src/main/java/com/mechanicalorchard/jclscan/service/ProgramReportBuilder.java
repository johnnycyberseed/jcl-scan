package com.mechanicalorchard.jclscan.service;

import java.util.List;

import org.springframework.stereotype.Component;

import com.mechanicalorchard.jclscan.model.ProgramReport;
import com.mechanicalorchard.jclscan.model.ProgramSummary;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
public class ProgramReportBuilder {

    public ProgramReport build(List<ProgramSummary> programSummaries) {
        log.info("Building program report ({} programs)", programSummaries.size());
        return new ProgramReport(programSummaries);
    }
}


