package com.mechanicalorchard.jclscan.service;

import lombok.extern.slf4j.Slf4j;

import org.springframework.stereotype.Component;

import com.mechanicalorchard.jclscan.model.UnresolvedRecord;
import com.mechanicalorchard.jclscan.model.UnresolvedReport;
import com.mechanicalorchard.jclscan.model.JclApp;
import com.mechanicalorchard.jclscan.model.JclStep;
import com.mechanicalorchard.jclscan.model.Job;
import com.mechanicalorchard.jclscan.model.Procedure;
import com.mechanicalorchard.jclscan.model.ProcedureRef;

import java.util.ArrayList;
import java.util.List;

@Slf4j
@Component
public class UnresolvedReportBuilder {

  public UnresolvedReport build(JclApp app) {
    log.info("Building unresolved report for {} jobs", app.getJobs().size());
    List<UnresolvedRecord> unresolvedItems = new ArrayList<>();
    for (Job job : app.getJobs()) {
      log.debug("Checking job {} for unresolved procedures", job.getName());
      collectUnresolvedProcedures(unresolvedItems, job.getName(), null, job.getSteps());
    }
    return UnresolvedReport.builder().unresolvedItems(unresolvedItems).build();
  }

  private void collectUnresolvedProcedures(List<UnresolvedRecord> out, String jobName, String prefix, List<JclStep> steps) {
    for (JclStep step : steps) {
      String stepPath = prefix == null ? step.getName() : prefix + "." + step.getName();
      
      // Check if this step has an unresolved procedure reference
      if (step.getProc() instanceof ProcedureRef procRef) {
        log.trace("Found unresolved procedure reference: {}", procRef.getName());
        out.add(UnresolvedRecord.builder()
            .jobName(jobName)
            .stepName(stepPath)
            .procedureName(procRef.getName())
            .programName("(unknown)")
            .build());
      }
      
      // If the step has a resolved procedure, recursively check its steps
      if (step.getProc() instanceof Procedure proc) {
        collectUnresolvedProcedures(out, jobName, stepPath, proc.getSteps());
      }
    }
  }
}
