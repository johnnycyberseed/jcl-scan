package com.mechanicalorchard.jclscan.service;

import java.io.IOException;
import java.nio.file.Path;

import com.mechanicalorchard.jclscan.model.ExecutionReport;
import com.mechanicalorchard.jclscan.model.ProgramReport;

public interface ReportWriter {
    void writeProgramReport(Path outputFile, ProgramReport report) throws IOException;
    void writeExecutionReport(Path outputFile, ExecutionReport report) throws IOException;
}


