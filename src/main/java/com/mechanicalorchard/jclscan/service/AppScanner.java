package com.mechanicalorchard.jclscan.service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.ArrayList;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mechanicalorchard.jclscan.model.AppSourceFile;
import com.mechanicalorchard.jclscan.model.ExecutionReport;
import com.mechanicalorchard.jclscan.model.JclApp;
import com.mechanicalorchard.jclscan.model.ProgramReport;
import com.mechanicalorchard.jclscan.model.ProgramSummary;
import com.mechanicalorchard.jclscan.model.UnresolvedReport;

@Service
public class AppScanner {

  @Autowired
  private ReportWriter reportWriter;

  @Autowired
  private ProgramReportBuilder programReportBuilder;

  @Autowired
  private ExecutionReportBuilder executionReportBuilder;

  @Autowired
  private UnresolvedReportBuilder unresolvedReportBuilder;

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
    // Discover application sources from provided inputs
    List<AppSourceFile> appSources = sourceDiscoverer.discover(inputPaths);
    // Also discover commonly catalogued procedures/programs from classpath libs
    List<AppSourceFile> systemSources = sourceDiscoverer.discover(List.of(Paths.get("classpath:libs")));

    // Parse into separate applications so we can keep distinct libraries and control precedence
    JclApp app = appParser.parse(appSources);
    JclApp system = appParser.parse(systemSources);

    // Resolve jobs using app libraries with precedence, followed by system libraries
    resolver.resolve(
        app.getJobs(),
        List.of(app.getProcLib(), system.getProcLib()),
        List.of(app.getLinkLib(), system.getLinkLib()));

    // Build program report from both app and system link libraries
    List<ProgramSummary> summaries = new ArrayList<>();
    summaries.addAll(app.getLinkLib().registered().stream()
        .map(p -> (ProgramSummary) p)
        .toList());
    summaries.addAll(system.getLinkLib().registered().stream()
        .map(p -> (ProgramSummary) p)
        .toList());
    ProgramReport programReport = programReportBuilder.build(summaries);
    ExecutionReport executionReport = executionReportBuilder.build(app);
    UnresolvedReport unresolvedReport = unresolvedReportBuilder.build(app);

    Files.createDirectories(outputDirectory);
    Path programReportOutputFile = outputDirectory.resolve("program-report.csv");
    Path executionReportOutputFile = outputDirectory.resolve("execution-report.csv");
    Path unresolvedReportOutputFile = outputDirectory.resolve("unresolved-report.csv");
    reportWriter.writeProgramReport(programReportOutputFile, programReport);
    reportWriter.writeExecutionReport(executionReportOutputFile, executionReport);
    reportWriter.writeUnresolvedReport(unresolvedReportOutputFile, unresolvedReport);
  }
}
