package com.mechanicalorchard.jclscan.service;

import org.springframework.stereotype.Service;

import com.mechanicalorchard.jclscan.model.Program;
import com.mechanicalorchard.jclscan.model.ProgramSummary;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
public class CobolAnalyzer {

  private static final Pattern PROGRAM_ID_PATTERN = Pattern.compile("PROGRAM-ID\\.\\s+([A-Z0-9\\-]+)");

  public ProgramSummary analyze(String fileName, String cobolContent, String libraryName) {
    String programName = extractProgramId(cobolContent);
    int linesOfCode = countLinesOfCode(cobolContent);
    
    return ProgramSummary.builder()
        .libraryName(libraryName)
        .fileName(fileName)
        .programName(programName)
        .kind(Program.Kind.COBOL)
        .linesOfCode(linesOfCode)
        .numberOfConditionals(0)
        .numberOfRoutines(0)
        .build();
  }

  private String extractProgramId(String cobolContent) {
    Matcher matcher = PROGRAM_ID_PATTERN.matcher(cobolContent);
    if (matcher.find()) {
      return matcher.group(1);
    }
    return null;
  }

  private int countLinesOfCode(String cobolContent) {
    return (int) cobolContent.lines()
        .filter(line -> !line.trim().isEmpty())
        .count();
  }
}
