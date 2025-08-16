package com.mechanicalorchard.jclscan.service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mechanicalorchard.jclscan.model.AppSourceFile;
import com.mechanicalorchard.jclscan.model.ExecutionReport;
import com.mechanicalorchard.jclscan.model.JclApp;
import com.mechanicalorchard.jclscan.model.ProgramReport;
import com.mechanicalorchard.jclscan.model.ProgramSummary;

@Service
public class AppScanner {

  @Autowired
  private ReportWriter reportWriter;

  @Autowired
  private ProgramReportBuilder programReportBuilder;

  @Autowired
  private ExecutionReportBuilder executionReportBuilder;

  @Autowired
  private AppParser appParser;

  @Autowired
  private Resolver resolver;

  @Autowired
  private SourceDiscoverer sourceDiscoverer;

  public void scan(Path programReportOutputFile) throws IOException {
    throw new IllegalArgumentException("No inputs provided. Use scan(outputDirectory, inputPaths).");
  }

  public void scan(Path outputDirectory, List<Path> inputPaths) throws IOException {
    List<AppSourceFile> sources = sourceDiscoverer.discover(inputPaths);

    JclApp app = appParser.parse(sources);
    resolver.resolve(app);

    List<ProgramSummary> summaries = app.getLinkLib().registered().stream()
        .map(p -> (ProgramSummary) p)
        .toList();
    ProgramReport programReport = programReportBuilder.build(summaries);
    ExecutionReport executionReport = executionReportBuilder.build(app);

    Files.createDirectories(outputDirectory);
    Path programReportOutputFile = outputDirectory.resolve("program-report.csv");
    Path executionReportOutputFile = outputDirectory.resolve("execution-report.csv");
    reportWriter.writeProgramReport(programReportOutputFile, programReport);
    reportWriter.writeExecutionReport(executionReportOutputFile, executionReport);
  }
}
