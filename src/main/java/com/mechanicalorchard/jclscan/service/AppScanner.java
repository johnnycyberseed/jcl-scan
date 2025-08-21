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
import com.mechanicalorchard.jclscan.model.Job;
import com.mechanicalorchard.jclscan.model.Procedure;
import com.mechanicalorchard.jclscan.model.Program;
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
    // Parse each user input root separately to maintain library boundaries
    List<JclApp> userApps = new ArrayList<>();
    for (int i = 0; i < inputPaths.size(); i++) {
      Path inputPath = inputPaths.get(i);
      List<AppSourceFile> sources = sourceDiscoverer.discover(List.of(inputPath));
      JclApp app = appParser.parse(sources);
      
      // Name the libraries for this user root
      String libraryName = "USR" + (i + 1) + ".LINKLIB";
      app.getLinkLib().setName(libraryName);
      app.getProcLib().setName("USR" + (i + 1) + ".PROCLIB");
      
      userApps.add(app);
    }

    // Collect all jobs from user apps for resolution
    List<Job> allJobs = userApps.stream()
        .flatMap(app -> app.getJobs().stream())
        .collect(ArrayList::new, ArrayList::add, ArrayList::addAll);

    // Collect procedure and link libraries in precedence order: user libs first
    List<com.mechanicalorchard.jclscan.model.Library<Procedure>> procLibs = new ArrayList<>();
    List<com.mechanicalorchard.jclscan.model.Library<Program>> linkLibs = new ArrayList<>();
    
    for (JclApp userApp : userApps) {
      procLibs.add(userApp.getProcLib());
      linkLibs.add(userApp.getLinkLib());
    }

    // Always load system libraries for complete analysis
    List<AppSourceFile> systemSources = sourceDiscoverer.discover(List.of(Paths.get("classpath:libs")));
    JclApp systemApp = appParser.parse(systemSources);
    systemApp.getLinkLib().setName("SYS1.LINKLIB");
    systemApp.getProcLib().setName("SYS1.PROCLIB");
    
    procLibs.add(systemApp.getProcLib());
    linkLibs.add(systemApp.getLinkLib());

    // Resolve jobs using libraries with precedence
    resolver.resolve(allJobs, procLibs, linkLibs);

    // Build program report by collecting all programs from all libraries and setting their library names
    List<ProgramSummary> summaries = new ArrayList<>();
    for (com.mechanicalorchard.jclscan.model.Library<Program> linkLib : linkLibs) {
      for (Program program : linkLib.registered()) {
        if (program instanceof ProgramSummary summary) {
          // Create a new ProgramSummary with the library name set
          ProgramSummary summaryWithLibrary = ProgramSummary.builder()
              .libraryName(linkLib.getName())
              .fileName(summary.getFileName())
              .programName(summary.getProgramName())
              .kind(summary.getKind())
              .linesOfCode(summary.getLinesOfCode())
              .numberOfConditionals(summary.getNumberOfConditionals())
              .numberOfRoutines(summary.getNumberOfRoutines())
              .build();
          summaries.add(summaryWithLibrary);
        }
      }
    }

    // Create a combined app for execution and unresolved reports (they expect a single JclApp with all jobs)
    JclApp combinedApp = new JclApp();
    for (JclApp userApp : userApps) {
      combinedApp.getJobs().addAll(userApp.getJobs());
    }
    // If no user apps, use system app (which might be null if no jobs)
    JclApp primaryApp = userApps.isEmpty() ? systemApp : combinedApp;
    
    ProgramReport programReport = programReportBuilder.build(summaries);
    ExecutionReport executionReport = executionReportBuilder.build(primaryApp);
    UnresolvedReport unresolvedReport = unresolvedReportBuilder.build(primaryApp);

    Files.createDirectories(outputDirectory);
    Path programReportOutputFile = outputDirectory.resolve("program-report.csv");
    Path executionReportOutputFile = outputDirectory.resolve("execution-report.csv");
    Path unresolvedReportOutputFile = outputDirectory.resolve("unresolved-report.csv");
    reportWriter.writeProgramReport(programReportOutputFile, programReport);
    reportWriter.writeExecutionReport(executionReportOutputFile, executionReport);
    reportWriter.writeUnresolvedReport(unresolvedReportOutputFile, unresolvedReport);
  }


}
