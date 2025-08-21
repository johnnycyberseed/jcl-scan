package com.mechanicalorchard.jclscan.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ExecutionRecord {
  private String jobName;
  private String stepName;
  private String procedureName;
  private String programName;
  private String libraryName;
  private Program.Kind programKind;
  private int linesOfCode;
}
