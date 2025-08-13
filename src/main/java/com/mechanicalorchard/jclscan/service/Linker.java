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

@Service
public class Linker {

  public void link(JclApp app) {
    for (Job job : app.getJobs()) {
      resolveSteps(app, job.getSteps());
    }
  }

  private void resolveSteps(JclApp app, List<JclStep> steps) {
    for (JclStep step : steps) {
      if (step.getProc() instanceof ProcedureRef procRef) {
        Procedure resolved = app.getProcLib().resolve(procRef.getName());
        if (resolved != null) {
          step.setProc(resolved);
          // Resolve nested steps inside the resolved procedure
          if (resolved.getSteps() != null) {
            resolveSteps(app, resolved.getSteps());
          }
        }
      }

      if (step.getPgm() instanceof ProgramRef progRef) {
        Program resolved = app.getLinkLib().resolve(progRef.getName());
        if (resolved != null) {
          step.setPgm(resolved);
        }
      }
    }
  }
}


