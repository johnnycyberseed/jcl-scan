package com.mechanicalorchard.jclscan;

import com.mechanicalorchard.jclscan.model.*;
import com.mechanicalorchard.jclscan.service.Linker;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Objects;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
class LinkerResolutionTests {

  @Autowired
  private Linker linker;

  @SuppressWarnings("null")
  @Test
  void link_resolvesProcedureAndProgramReferencesInPlace() {
    // Build JclApp with one job, one procedure, and two programs
    JclApp app = new JclApp();

    // Register procedure DAILYDO with two steps
    Procedure dailyDo = Procedure.builder()
        .name("DAILYDO")
        .steps(List.of(
            JclStep.builder().name("DOTHING").pgm(ProgramRef.builder().name("PAYROLL1").build()).build(),
            JclStep.builder().name("RPTTHING").pgm(ProgramRef.builder().name("EZT1").build()).build()
        ))
        .build();
    app.getProcLib().register("DAILYDO", dailyDo);

    // Register programs in link lib
    app.getLinkLib().register("PAYROLL1", ProgramSummary.builder()
        .fileName("PAYROLL1.cbl")
        .programName("PAYROLL1")
        .kind(Program.Kind.COBOL)
        .linesOfCode(4)
        .numberOfConditionals(0)
        .numberOfRoutines(0)
        .build());

    app.getLinkLib().register("EZT1", ProgramSummary.builder()
        .fileName("EZT1.ezt")
        .programName("EZT1")
        .kind(Program.Kind.EASYTRIEVE)
        .linesOfCode(5)
        .numberOfConditionals(0)
        .numberOfRoutines(0)
        .build());

    // Create job DAILY01 that EXECs procedure DAILYDO
    Job job = Job.builder()
        .name("DAILY01")
        .steps(List.of(
            JclStep.builder().name("STEP01").proc(ProcedureRef.builder().name("DAILYDO").build()).build()
        ))
        .build();
    app.getJobs().add(job);

    // When
    linker.link(app);

    // Then: the job's step should now reference a resolved Procedure, not a ProcedureRef
    JclStep step01 = app.getJobs().get(0).getSteps().get(0);
    assertThat(step01.getProc()).isInstanceOf(Procedure.class);
    assertThat(step01.getProc()).isNotInstanceOf(ProcedureRef.class);
    Procedure resolvedProc = (Procedure) step01.getProc();
    assertThat(resolvedProc.getName()).isEqualTo("DAILYDO");

    // And inside the resolved procedure, program references should be resolved
    JclStep doThing = resolvedProc.getSteps().get(0);
    assertThat(doThing.getPgm()).isNotNull();
    assertThat(doThing.getPgm()).isInstanceOf(ProgramSummary.class);
    assertThat(((ProgramSummary) doThing.getPgm()).getProgramName()).isEqualTo("PAYROLL1");

    JclStep rptThing = resolvedProc.getSteps().get(1);
    assertThat(rptThing.getPgm()).isNotNull();
    assertThat(rptThing.getPgm()).isInstanceOf(ProgramSummary.class);
    assertThat(((ProgramSummary) rptThing.getPgm()).getProgramName()).isEqualTo("EZT1");
  }
}


