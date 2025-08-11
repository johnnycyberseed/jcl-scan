package com.mechanicalorchard.jclscan.service;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mechanicalorchard.jclscan.model.AppSourceFile;
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
  private JclAppParserService jclAppParserService;

  @Autowired
  private SourceDiscoverer sourceDiscoverer;

  public void scan(Path programReportOutputFile) throws IOException {
    throw new IllegalArgumentException("No inputs provided. Use scan(output, inputPaths).");
  }

  public void scan(Path programReportOutputFile, List<Path> inputPaths) throws IOException {
    List<AppSourceFile> sources = sourceDiscoverer.discover(inputPaths);
    JclApp app = jclAppParserService.parse(sources);
    List<ProgramSummary> summaries = app.getLinkLib().registered().stream()
        .map(p -> (ProgramSummary) p)
        .toList();
    ProgramReport report = programReportBuilder.build(summaries);
    reportWriter.writeProgramReport(programReportOutputFile, report);
  }
}
