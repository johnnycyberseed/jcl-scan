package com.mechanicalorchard.jclscan.service;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import org.springframework.stereotype.Service;

import com.mechanicalorchard.jclscan.model.ProgramReport;
import com.mechanicalorchard.jclscan.model.ProgramSummary;

@Service
public class AppService {

    private final ReportWriter reportWriter;

    public AppService(ReportWriter reportWriter) {
        this.reportWriter = reportWriter;
    }

    public void scan(Path programReportOutputFile) throws IOException {
        ProgramReport report = new ProgramReport(List.of(
                new ProgramSummary("PAYROLL1.cbl", "PAYROLL1", "COBOL", 123, 10, 3),
                new ProgramSummary("EZT1.ezt", "EZT1", "Easytrieve", 200, 8, 5)));

        reportWriter.writeProgramReport(programReportOutputFile, report);
    }
}


