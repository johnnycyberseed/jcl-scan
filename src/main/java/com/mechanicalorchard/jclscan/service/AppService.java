package com.mechanicalorchard.jclscan.service;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mechanicalorchard.jclscan.model.ProgramReport;
import com.mechanicalorchard.jclscan.model.ProgramSummary;

@Service
public class AppService {

    @Autowired
    private ReportWriter reportWriter;

    @Autowired
    private ProgramReportBuilder programReportBuilder;

    public void scan(Path programReportOutputFile, List<ProgramSummary> summaries) throws IOException {
        ProgramReport report = programReportBuilder.build(summaries);
        reportWriter.writeProgramReport(programReportOutputFile, report);
    }
}


