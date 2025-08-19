package com.mechanicalorchard.jclscan.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class UnresolvedRecord {
  private String jobName;
  private String stepName;
  private String procedureName;
  private String programName;
}
