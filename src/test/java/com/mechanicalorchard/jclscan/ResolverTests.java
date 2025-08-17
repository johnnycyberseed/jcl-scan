package com.mechanicalorchard.jclscan;

import com.mechanicalorchard.jclscan.model.*;
import com.mechanicalorchard.jclscan.service.Resolver;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
class ResolverTests {

  @Autowired
  private Resolver resolver;

  private JclApp buildSampleApp() {
    JclApp app = new JclApp();

    Job job = Job.builder()
        .name("DAILY01")
        .steps(List.of(
            JclStep.builder().name("STEP01").proc(ProcedureRef.builder().name("DAILYDO").build()).build()
        ))
        .build();
    app.getJobs().add(job);

    Procedure dailyDo = Procedure.builder()
        .name("DAILYDO")
        .steps(List.of(
            JclStep.builder().name("DOTHING").pgm(ProgramRef.builder().name("PAYROLL1").build()).build(),
            JclStep.builder().name("RPTTHING").pgm(ProgramRef.builder().name("EZT1").build()).build()
        ))
        .build();
    app.getProcLib().register("DAILYDO", dailyDo);

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

    return app;
  }

  @SuppressWarnings("null")
  @Test
  void resolve_resolvesJobStepProcReference_toProcedure() {
    JclApp app = buildSampleApp();

    resolver.resolve(app);

    JclStep step01 = app.getJobs().get(0).getSteps().get(0);
    assertThat(step01.getProc()).isInstanceOf(Procedure.class);
    assertThat(step01.getProc()).isNotInstanceOf(ProcedureRef.class);
    Procedure resolvedProc = (Procedure) step01.getProc();
    assertThat(resolvedProc.getName()).isEqualTo("DAILYDO");
  }

  @SuppressWarnings("null")
  @Test
  void resolve_resolvesProcedureProgramReferences_toPrograms() {
    JclApp app = buildSampleApp();

    resolver.resolve(app);

    Procedure resolvedProc = (Procedure) app.getJobs().get(0).getSteps().get(0).getProc();

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


