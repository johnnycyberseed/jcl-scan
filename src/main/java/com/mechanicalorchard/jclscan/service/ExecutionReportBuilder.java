package com.mechanicalorchard.jclscan.service;

import lombok.extern.slf4j.Slf4j;

import org.springframework.stereotype.Component;

import com.mechanicalorchard.jclscan.model.ExecutionRecord;
import com.mechanicalorchard.jclscan.model.ExecutionReport;
import com.mechanicalorchard.jclscan.model.JclApp;
import com.mechanicalorchard.jclscan.model.JclStep;
import com.mechanicalorchard.jclscan.model.Job;
import com.mechanicalorchard.jclscan.model.Procedure;
import com.mechanicalorchard.jclscan.model.ProgramSummary;

import java.util.ArrayList;
import java.util.List;

@Slf4j
@Component
public class ExecutionReportBuilder {

  public ExecutionReport build(JclApp app) {
    log.info("Building execution report for {} jobs", app.getJobs().size());
    List<ExecutionRecord> rows = new ArrayList<>();
    for (Job job : app.getJobs()) {
      log.debug("Reporting on job {}", job.getName());
      collect(rows, job.getName(), null, job.getSteps());
    }
    return ExecutionReport.builder().executions(rows).build();
  }

  private void collect(List<ExecutionRecord> out, String jobName, String prefix, List<JclStep> steps) {
    for (JclStep step : steps) {
      log.trace("Reporting on step {}", step.getName());
      String stepPath = prefix == null ? step.getName() : prefix + "." + step.getName();
      if (step.getProc() instanceof Procedure proc) {
        if (proc.getSteps() != null) {
          collect(out, jobName, stepPath, proc.getSteps());
        }
      }
      if (step.getPgm() instanceof ProgramSummary summary) {
        out.add(ExecutionRecord.builder()
            .jobName(jobName)
            .stepName(stepPath)
            .procedureName("(program)")
            .programName(baseName(summary.getFileName()))
            .programKind(summary.getKind())
            .linesOfCode(summary.getLinesOfCode())
            .build());
      }
    }
  }

  private String baseName(String fileName) {
    int dot = fileName.lastIndexOf('.');
    return dot > 0 ? fileName.substring(0, dot) : fileName;
  }
}
