package com.mechanicalorchard.jclscan.model;

import java.util.List;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ExecutionReport {
  private List<ExecutionRecord> executions;
}
