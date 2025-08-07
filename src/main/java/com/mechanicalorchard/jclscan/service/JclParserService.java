package com.mechanicalorchard.jclscan.service;

import com.mechanicalorchard.jclscan.model.JclFile;
import com.mechanicalorchard.jclscan.model.JclStep;
import com.mechanicalorchard.jclscan.model.ProcRef;
import com.mechanicalorchard.jclscan.model.ProgRef;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.springframework.stereotype.Service;

@Service
public class JclParserService {

  private static final Pattern JOB_PATTERN = Pattern.compile("^//([A-Z0-9]+)\\s+JOB\\s+.*",
      Pattern.MULTILINE);
  private static final Pattern PROC_PATTERN = Pattern.compile("^//([A-Z0-9]+)\\s+PROC\\s*.*",
      Pattern.MULTILINE);
  private static final Pattern STEP_PATTERN = Pattern.compile("^//([A-Z0-9]+)\\s+EXEC\\s+(.*)$", Pattern.MULTILINE);
  private static final Pattern PGM_PARAM_PATTERN = Pattern.compile("\\bPGM=([A-Z0-9]+)\\b");
  private static final Pattern PROC_PARAM_PATTERN = Pattern.compile("\\bPROC=([A-Z0-9]+)\\b");
  private static final Pattern PARAM_PATTERN = Pattern.compile("\\b([A-Z0-9]+)=((?:\\([^)]*\\)|[^,\\s])+)");

  public JclFile parseJclFile(String jclContent) {
    // Preprocess to handle continuation lines
    String normalizedContent = joinContinuationLines(jclContent);

    boolean isJob = isJobOrProc(normalizedContent);
    String fileName = extractJobOrProcName(normalizedContent);
    List<JclStep> steps = extractSteps(normalizedContent);

    return JclFile.builder()
        .name(fileName)
        .isJob(isJob)
        .steps(steps)
        .build();
  }

  private boolean isJobOrProc(String jclContent) {
    Matcher matcher = JOB_PATTERN.matcher(jclContent);
    return matcher.find();
  }

  private String extractJobOrProcName(String jclContent) {
    Matcher matcher = JOB_PATTERN.matcher(jclContent);
    String name = null;
    if (matcher.find()) {
      name = matcher.group(1);
    }

    matcher = PROC_PATTERN.matcher(jclContent);
    if (matcher.find()) {
      name = matcher.group(1);
    }
    if (name == null) {
      throw new IllegalArgumentException("No JOB or PROC statement found in JCL content");
    }
    return name;
  }

  private List<JclStep> extractSteps(String jclContent) {
    List<JclStep> steps = new ArrayList<>();
    Matcher matcher = STEP_PATTERN.matcher(jclContent);

    while (matcher.find()) {
      String stepName = matcher.group(1);
      String parameterList = matcher.group(2); // All parameters after EXEC

      // Check for first positional argument (procedure name without PROC= prefix)
      String firstPositionalProc = extractFirstPositionalArgument(parameterList);
      
      // Extract PGM parameter if present
      String pgmName = extractParameter(parameterList, PGM_PARAM_PATTERN);
      ProgRef progRef = pgmName != null ? ProgRef.builder().name(pgmName).build() : null;

      // Extract PROC parameter if present (named PROC= takes precedence over positional)
      String procName = extractParameter(parameterList, PROC_PARAM_PATTERN);
      ProcRef procRef = null;
      if (procName != null) {
        procRef = ProcRef.builder().name(procName).build();
      } else if (firstPositionalProc != null) {
        procRef = ProcRef.builder().name(firstPositionalProc).build();
      }

      // Extract symbolic parameters (everything except PGM and PROC)
      Map<String, String> symbolicParams = extractSymbolicParameters(parameterList);

      steps.add(JclStep.builder()
          .name(stepName)
          .pgm(progRef)
          .proc(procRef)
          .symbolicParameters(symbolicParams)
          .build());
    }

    return steps;
  }

  private String extractParameter(String parameterList, Pattern paramPattern) {
    Matcher paramMatcher = paramPattern.matcher(parameterList);
    return paramMatcher.find() ? paramMatcher.group(1) : null;
  }

  private String extractFirstPositionalArgument(String parameterList) {
    // Split by comma and get the first parameter
    String[] parameters = parameterList.trim().split(",");
    if (parameters.length > 0) {
      String firstParam = parameters[0].trim();
      // If the first parameter doesn't contain '=', it's a positional argument
      if (!firstParam.contains("=") && firstParam.matches("[A-Z0-9]+")) {
        return firstParam;
      }
    }
    return null;
  }

  private Map<String, String> extractSymbolicParameters(String parameterList) {
    Map<String, String> symbolicParams = new HashMap<>();
    Matcher matcher = PARAM_PATTERN.matcher(parameterList);
    
    while (matcher.find()) {
      String paramName = matcher.group(1);
      String paramValue = matcher.group(2);
      
      // Skip first-class parameters (PGM, PROC, and COND)
      if (!"PGM".equals(paramName) && !"PROC".equals(paramName) && !"COND".equals(paramName)) {
        symbolicParams.put(paramName, paramValue);
      }
    }
    
    return symbolicParams;
  }

  private String joinContinuationLines(String jclContent) {
    String[] lines = jclContent.split("\n");
    StringBuilder result = new StringBuilder();
    StringBuilder currentStatement = new StringBuilder();

    for (String line : lines) {
      switch (classifyLine(line)) {
        case EMPTY -> {
          flushCurrentStatement(result, currentStatement);
          result.append("\n");
        }
        case CONTINUATION -> {
          String continuationContent = line.substring(2).trim();
          if (!currentStatement.isEmpty()) {
            stripTrailing(currentStatement, " ,");
            currentStatement.append(",").append(continuationContent);
          } else {
            // Standalone continuation line (shouldn't happen, but handle gracefully)
            currentStatement.append(continuationContent);
          }
        }
        case JCL_STATEMENT -> {
          flushCurrentStatement(result, currentStatement);
          currentStatement.append(line);
        }
        case NON_JCL -> {
          // Be tolerant; we're not parsing JCL, per se.
          flushCurrentStatement(result, currentStatement);
          result.append(line).append("\n");
        }
      }
    }

    // Don't forget the last statement
    if (!currentStatement.isEmpty()) {
      result.append(currentStatement.toString().trim()).append("\n");
    }

    return result.toString();
  }

  private enum LineType {
    EMPTY, CONTINUATION, JCL_STATEMENT, NON_JCL
  }

  private LineType classifyLine(String line) {
    if (line.trim().isEmpty()) {
      return LineType.EMPTY;
    } else if (line.startsWith("//") && line.length() > 2 && line.charAt(2) == ' ') {
      return LineType.CONTINUATION;
    } else if (line.startsWith("//")) {
      return LineType.JCL_STATEMENT;
    } else {
      return LineType.NON_JCL;
    }
  }

  private void flushCurrentStatement(StringBuilder result, StringBuilder currentStatement) {
    if (!currentStatement.isEmpty()) {
      result.append(currentStatement.toString().trim()).append("\n");
      currentStatement.setLength(0);
    }
  }

  // Remove trailing characters if they are one of `trimChars`.
  private void stripTrailing(StringBuilder sb, String trimChars) {
    while (sb.length() > 0 && trimChars.indexOf(sb.charAt(sb.length() - 1)) != -1) {
      sb.setLength(sb.length() - 1);
    }
  }
}