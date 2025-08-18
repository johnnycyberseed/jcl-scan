package com.mechanicalorchard.jclscan.service;

import com.mechanicalorchard.jclscan.model.JclStep;
import com.mechanicalorchard.jclscan.model.Job;
import com.mechanicalorchard.jclscan.model.Library;
import com.mechanicalorchard.jclscan.model.Procedure;
import com.mechanicalorchard.jclscan.model.ProcedureRef;
import com.mechanicalorchard.jclscan.model.Program;
import com.mechanicalorchard.jclscan.model.ProgramRef;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class Resolver {

  public void resolve(List<Job> jobs, List<Library<Procedure>> jclLib, List<Library<Program>> jobLib) {
    log.info("JCL app has {} job(s), {} procedure(s), and {} program(s)",
        jobs.size(),
        jclLib.size(),
        jobLib.size());
    for (Job job : jobs) {
      log.debug("Processing job {}", job.getName());
      resolveSteps(jclLib, jobLib, job.getName(), job.getSteps());
    }
  }

  private void resolveSteps(List<Library<Procedure>> jclLib, List<Library<Program>> jobLib, String jobName, List<JclStep> steps) {
    for (JclStep step : steps) {
      log.debug("Processing step {}.{}", jobName, step.getName());
      if (step.getProc() instanceof ProcedureRef procRef) {
        log.trace("Resolving procedure reference \"{}\"", procRef.getName());
        Procedure resolved = jclLib.stream().map(l -> l.resolve(procRef.getName())).filter(Objects::nonNull).findFirst().orElse(null);
        if (resolved != null) {
          log.trace("Resolved \"{}\" to {}", procRef.getName(), resolved);
          // Instantiate a per-invocation copy of the procedure with symbolic parameters
          // applied
          Map<String, String> effectiveParams = mergeSymbolicParameters(
              resolved.getSymbolicParameterDefaults(),
              step.getSymbolicParameters());
          Procedure instantiated = copyWithParams(resolved, effectiveParams);
          step.setProc(instantiated);
          String stepPrefix = jobName + "." + step.getName();
          resolveSteps(jclLib, jobLib, stepPrefix, instantiated.getSteps());
        } else {
          log.warn("Unresolved procedure reference \"{}\"", procRef.getName());
        }
      }

      if (step.getPgm() instanceof ProgramRef progRef) {
        Program resolved = jobLib.stream().map(l -> l.resolve(progRef.getName())).filter(Objects::nonNull).findFirst().orElse(null);
        if (resolved != null) {
          log.trace("Resolved \"{}\" to {}", progRef.getName(), resolved);
          step.setPgm(resolved);
        } else {
          log.warn("Unresolved program reference \"{}\"", progRef.getName());
        }
      }
    }
  }

  private Map<String, String> mergeSymbolicParameters(Map<String, String> defaults, Map<String, String> overrides) {
    Map<String, String> merged = new HashMap<>();
    if (defaults != null) {
      merged.putAll(defaults);
    }
    if (overrides != null) {
      merged.putAll(overrides);
    }
    return merged;
  }

  private Procedure copyWithParams(Procedure resolvedProc, Map<String, String> params) {
    List<JclStep> clonedSteps = new ArrayList<>();
    for (JclStep step : resolvedProc.getSteps()) {
      // Create steps based on symbolic substitution behavior
      List<JclStep> expanded = expandStepForSymbolicParameters(resolvedProc, step, params);
      clonedSteps.addAll(expanded);
    }

    // Carry forward defaults for transparency; keep template defaults
    return Procedure.builder()
        .name(resolvedProc.getName())
        .symbolicParameterDefaults(resolvedProc.getSymbolicParameterDefaults())
        .steps(clonedSteps)
        .build();
  }

  private List<JclStep> expandStepForSymbolicParameters(Procedure resolvedProc, JclStep step, Map<String, String> params) {
    List<JclStep> result = new ArrayList<>();

    // Handle program substitution (primary focus for symbolic params)
    if (step.getPgm() instanceof ProgramRef progRef && progRef.getName() != null
        && progRef.getName().startsWith("&")) {
      String key = progRef.getName().substring(1);
      String defaultValue = resolvedProc.getSymbolicParameterDefaults() != null
          ? resolvedProc.getSymbolicParameterDefaults().get(key)
          : null;
      String overrideValue = params != null ? params.get(key) : null;

      // If both default and override are present and non-blank and different,
      // emit two steps: default then override. Treat blank defaults as absent.
      if (hasText(overrideValue) && hasText(defaultValue) && !overrideValue.equals(defaultValue)) {
        // Default-bound step
        result.add(buildClonedStep(step, ProgramRef.builder().name(defaultValue).build(),
            substituteProc(step.getProc(), params)));
        // Override-bound step
        result.add(buildClonedStep(step, ProgramRef.builder().name(overrideValue).build(),
            substituteProc(step.getProc(), params)));
        return result;
      }

      // Single substitution path
      String singleValue = hasText(overrideValue) ? overrideValue : (hasText(defaultValue) ? defaultValue : null);
      if (singleValue != null) {
        result.add(buildClonedStep(step, ProgramRef.builder().name(singleValue).build(),
            substituteProc(step.getProc(), params)));
        return result;
      }
      // Fall-through: no substitution value found, keep as-is
    }

    // Non-symbolic program or no values: apply simple substitution and clone once
    Program substitutedProgram = substituteProgram(step.getPgm(), params);
    var substitutedProc = substituteProc(step.getProc(), params);
    result.add(buildClonedStep(step, substitutedProgram, substitutedProc));
    return result;
  }

  private boolean hasText(String value) {
    return value != null && !value.trim().isEmpty();
  }

  private JclStep buildClonedStep(JclStep templateStep, Program program,
      com.mechanicalorchard.jclscan.model.JclScript proc) {
    return JclStep.builder()
        .name(templateStep.getName())
        .pgm(program)
        .proc(proc)
        .symbolicParameters(templateStep.getSymbolicParameters())
        .build();
  }

  private Program substituteProgram(Program program, Map<String, String> params) {
    if (!(program instanceof ProgramRef progRef)) {
      return program;
    }
    String name = progRef.getName();
    if (name != null && name.startsWith("&")) {
      String key = name.substring(1);
      String resolvedName = params.get(key);
      if (resolvedName != null && !resolvedName.isEmpty()) {
        return ProgramRef.builder().name(resolvedName).build();
      }
    }
    return program;
  }

  private com.mechanicalorchard.jclscan.model.JclScript substituteProc(
      com.mechanicalorchard.jclscan.model.JclScript proc,
      Map<String, String> params) {
    if (!(proc instanceof ProcedureRef pr)) {
      return proc;
    }
    String name = pr.getName();
    if (name != null && name.startsWith("&")) {
      String key = name.substring(1);
      String resolvedName = params.get(key);
      if (resolvedName != null && !resolvedName.isEmpty()) {
        return ProcedureRef.builder().name(resolvedName).build();
      }
    }
    return proc;
  }
}
