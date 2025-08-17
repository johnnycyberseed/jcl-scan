package com.mechanicalorchard.jclscan.service;

import com.mechanicalorchard.jclscan.model.Procedure;
import com.mechanicalorchard.jclscan.model.JclScript;
import com.mechanicalorchard.jclscan.model.Job;
import com.mechanicalorchard.jclscan.model.JclStep;
import com.mechanicalorchard.jclscan.model.ProcedureRef;
import com.mechanicalorchard.jclscan.model.ProgramRef;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.springframework.stereotype.Service;

@Service
public class JclParser {

  private static final Pattern JOB_PATTERN = Pattern.compile("^//([A-Z0-9@]+)\\s+JOB\\s+.*",
      Pattern.MULTILINE);
  private static final Pattern PROC_PATTERN = Pattern.compile("^//([A-Z0-9@]+)\\s+PROC\\s*.*",
      Pattern.MULTILINE);
  private static final Pattern PROC_STATEMENT_PATTERN = Pattern.compile("^//[A-Z0-9@]+\\s+PROC\\s*(.*)$",
      Pattern.MULTILINE);
  private static final Pattern STEP_PATTERN = Pattern.compile("^//([A-Z0-9]+)\\s+EXEC\\s+(.*)$", Pattern.MULTILINE);

  // JCL first-class/keyword parameters that should not be captured as symbolic parameters
  private static final Set<String> EXEC_KEYWORD_PARAMETERS = Set.of("PGM", "PROC", "COND");

  public JclScript parseJclFile(String jclContent) {
    // Preprocess to handle continuation lines
    String normalizedContent = joinContinuationLines(jclContent);

    String fileName = extractJobOrProcName(normalizedContent);
    List<JclStep> steps = extractSteps(normalizedContent);

    if (isJob(normalizedContent)) {
      return Job.builder()
          .name(fileName)
          .steps(steps)
          .build();
    } else {
      Map<String, String> procDefaults = extractProcedureDefaults(normalizedContent);
      return Procedure.builder()
          .name(fileName)
          .symbolicParameterDefaults(procDefaults)
          .steps(steps)
          .build();
    }
  }

  private boolean isJob(String jclContent) {
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

      List<String> tokens;
      try {
        tokens = splitTopLevelParameters(parameterList);
      } catch (IllegalArgumentException e) {
        throw new IllegalArgumentException("While parsing step " + stepName + " in \n```jcl\n" + jclContent + "```", e);
      }

      // First positional argument: procedure name (if present and not name=value)
      String firstPositionalProc = extractFirstPositionalArgument(tokens);

      // Extract named parameters
      String pgmName = extractNamedParameterValue(tokens, "PGM");
      ProgramRef progRef = pgmName != null ? ProgramRef.builder().name(pgmName).build() : null;

      String procName = extractNamedParameterValue(tokens, "PROC");
      ProcedureRef procRef = null;
      if (procName != null) {
        procRef = ProcedureRef.builder().name(procName).build();
      } else if (firstPositionalProc != null) {
        procRef = ProcedureRef.builder().name(firstPositionalProc).build();
      }

      // Extract symbolic parameters (everything except PGM and PROC)
      Map<String, String> symbolicParams = extractSymbolicParameters(tokens);

      steps.add(JclStep.builder()
          .name(stepName)
          .pgm(progRef)
          .proc(procRef)
          .symbolicParameters(symbolicParams)
          .build());
    }

    return steps;
  }

  private String extractNamedParameterValue(List<String> tokens, String name) {
    String prefix = name + "=";
    for (String token : tokens) {
      String trimmed = token.trim();
      if (trimmed.regionMatches(true, 0, prefix, 0, prefix.length())) {
        String value = trimmed.substring(prefix.length()).trim();
        return normalizeValue(value);
      }
    }
    return null;
  }

  private String extractFirstPositionalArgument(List<String> tokens) {
    for (String token : tokens) {
      String trimmed = token.trim();
      if (!trimmed.contains("=")) {
        // Positional PROC name is unadorned token
        if (trimmed.matches("[A-Z0-9@&_.$#-]+")) {
          return trimmed;
        }
      }
      // If first token is name=value, there is no positional argument
      break;
    }
    return null;
  }

  private Map<String, String> extractSymbolicParameters(List<String> tokens) {
    Map<String, String> symbolicParams = new HashMap<>();
    for (String token : tokens) {
      String trimmed = token.trim();
      int eq = trimmed.indexOf('=');
      if (eq <= 0) {
        continue; // positional or malformed
      }
      String paramName = trimmed.substring(0, eq).trim();
      String rawValue = trimmed.substring(eq + 1).trim();

      // Skip first-class parameters by membership
      if (EXEC_KEYWORD_PARAMETERS.contains(paramName.toUpperCase())) {
        continue;
      }

      symbolicParams.put(paramName, normalizeValue(rawValue));
    }
    return symbolicParams;
  }

  private String normalizeValue(String value) {
    if (value.length() >= 2) {
      char first = value.charAt(0);
      char last = value.charAt(value.length() - 1);
      // Strip surrounding double quotes only; keep single quotes literal
      if (first == '"' && last == '"') {
        return value.substring(1, value.length() - 1);
      }
    }
    return value;
  }

  private List<String> splitTopLevelParameters(String parameterList) {
    List<String> tokens = new ArrayList<>();
    StringBuilder currToken = new StringBuilder();
    int parenDepth = 0;
    char inQuote = '\0'; // either the kind of quote we're in, or '\0' if we're not in a quoted string

    for (char c : parameterList.toCharArray()) {
      TokenizeAction action = (inQuote != '\0') ? TokenizeAction.INSIDE_QUOTE
          : switch (c) {
            case '\'', '"' -> TokenizeAction.ENTER_QUOTE;
            case '(' -> TokenizeAction.OPEN_PAREN;
            case ')' -> TokenizeAction.CLOSE_PAREN;
            case ',' -> (parenDepth == 0) ? TokenizeAction.COMMA_AT_TOP : TokenizeAction.APPEND_CHAR;
            default -> TokenizeAction.APPEND_CHAR;
          };

      switch (action) {
        case INSIDE_QUOTE -> {
          if (c == inQuote) {
            inQuote = '\0';
          }
          currToken.append(c);
        }
        case ENTER_QUOTE -> {
          inQuote = c;
          currToken.append(c);
        }
        case OPEN_PAREN -> {
          parenDepth++;
          currToken.append(c);
        }
        case CLOSE_PAREN -> {
          parenDepth--;
          if (parenDepth < 0) {
            throw new IllegalArgumentException(
                "Unbalanced parentheses (too many closing?) in parameter list: «" + parameterList + "»");
          }
          currToken.append(c);
        }
        case COMMA_AT_TOP -> {
          tokens.add(currToken.toString());
          currToken.setLength(0);
        }
        case APPEND_CHAR -> {
          currToken.append(c);
          // no state change needed beyond appending the character
        }
      }
    }
    if (currToken.length() > 0) {
      tokens.add(currToken.toString());
    }
    if (parenDepth > 0) {
      throw new IllegalArgumentException(
          "Unbalanced parentheses (not enough closing?) in parameter list: «" + parameterList + "»");
    }
    return tokens;
  }

  private enum TokenizeAction {
    INSIDE_QUOTE, ENTER_QUOTE, OPEN_PAREN, CLOSE_PAREN, COMMA_AT_TOP, APPEND_CHAR
  }

  private Map<String, String> extractProcedureDefaults(String jclContent) {
    Map<String, String> defaults = new HashMap<>();
    Matcher matcher = PROC_STATEMENT_PATTERN.matcher(jclContent);
    if (matcher.find()) {
      String parameterList = matcher.group(1);
      if (parameterList != null && !parameterList.isBlank()) {
        defaults.putAll(extractSymbolicParametersFromString(parameterList));
      }
    }
    return defaults;
  }

  private Map<String, String> extractSymbolicParametersFromString(String parameterList) {
    List<String> tokens = splitTopLevelParameters(parameterList);
    return extractSymbolicParameters(tokens);
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