package com.mechanicalorchard.jclscan.service;

import com.mechanicalorchard.jclscan.model.JclStep;
import com.mechanicalorchard.jclscan.model.JclApp;
import com.mechanicalorchard.jclscan.model.Job;
import com.mechanicalorchard.jclscan.model.Procedure;
import com.mechanicalorchard.jclscan.model.ProcedureRef;
import com.mechanicalorchard.jclscan.model.Program;
import com.mechanicalorchard.jclscan.model.ProgramRef;
import org.springframework.stereotype.Service;

import java.util.List;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class Resolver {

  public void resolve(JclApp app) {
    log.info("JCL app has {} job(s), {} procedure(s), and {} program(s)",
        app.getJobs().size(),
        app.getProcLib().registered().size(),
        app.getLinkLib().registered().size());
    for (Job job : app.getJobs()) {
      log.debug("Processing job {}", job.getName());
      resolveSteps(app, job.getName(), job.getSteps());
    }
  }

  private void resolveSteps(JclApp app, String jobName, List<JclStep> steps) {
    for (JclStep step : steps) {
      log.debug("Processing step {}.{}", jobName, step.getName());
      if (step.getProc() instanceof ProcedureRef procRef) {
        log.trace("Resolving procedure reference \"{}\"", procRef.getName());
        Procedure resolved = app.getProcLib().resolve(procRef.getName());
        if (resolved != null) {
          log.trace("Resolved \"{}\" to {}", procRef.getName(), resolved);
          step.setProc(resolved);
          if (resolved.getSteps() != null) {
            String stepPrefix = jobName + "." + step.getName();
            resolveSteps(app, stepPrefix, resolved.getSteps());
          }
        } else {
          log.warn("Unresolved procedure reference \"{}\"", procRef.getName());
        }
      }

      if (step.getPgm() instanceof ProgramRef progRef) {
        Program resolved = app.getLinkLib().resolve(progRef.getName());
        if (resolved != null) {
          log.trace("Resolved \"{}\" to {}", progRef.getName(), resolved);
          step.setPgm(resolved);
        } else {
          log.warn("Unresolved program reference \"{}\"", progRef.getName());
        }
      }
    }
  }
}
