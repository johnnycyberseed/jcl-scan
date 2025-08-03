package com.mechanicalorchard.jclscan.service;

import org.springframework.stereotype.Service;

import com.mechanicalorchard.jclscan.model.CobolFile;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
public class CobolAnalyzerService {

  private static final Pattern PROGRAM_ID_PATTERN = Pattern.compile("PROGRAM-ID\\.\\s+([A-Z0-9\\-]+)");

  public CobolFile analyze(String cobolName, String cobolContent) {
    String programName = extractProgramId(cobolContent);
    int linesOfCode = countLinesOfCode(cobolContent);
    
    return CobolFile.builder()
        .id(cobolName)
        .name(programName)
        .linesOfCode(linesOfCode)
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
