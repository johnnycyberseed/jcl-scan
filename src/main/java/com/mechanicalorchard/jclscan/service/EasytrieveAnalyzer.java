package com.mechanicalorchard.jclscan.service;

import org.springframework.stereotype.Service;

import com.mechanicalorchard.jclscan.model.Program;
import com.mechanicalorchard.jclscan.model.ProgramSummary;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
public class EasytrieveAnalyzer {

  private static final Pattern REPORT_PATTERN = Pattern.compile("REPORT\\s+([A-Z0-9\\-]+)");

  public ProgramSummary analyze(String easytrieveName, String easytrieveContent) {
    String reportName = extractReportName(easytrieveContent);
    int linesOfCode = countLinesOfCode(easytrieveContent);
    
    return ProgramSummary.builder()
        .fileName(easytrieveName)
        .programName(reportName)
        .kind(Program.Kind.EASYTRIEVE)
        .linesOfCode(linesOfCode)
        .numberOfConditionals(0)
        .numberOfRoutines(0)
        .build();
  }

  private String extractReportName(String easytrieveContent) {
    Matcher matcher = REPORT_PATTERN.matcher(easytrieveContent);
    if (matcher.find()) {
      return matcher.group(1);
    }
    return null;
  }

  private int countLinesOfCode(String easytrieveContent) {
    return (int) easytrieveContent.lines()
        .filter(line -> !line.trim().isEmpty())
        .count();
  }
}