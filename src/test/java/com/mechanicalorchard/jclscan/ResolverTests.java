package com.mechanicalorchard.jclscan;

import com.mechanicalorchard.jclscan.model.*;
import com.mechanicalorchard.jclscan.service.Resolver;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
class ResolverTests {

  @Autowired
  private Resolver resolver;

  private JclApp setupSimpleJclApp() {
    JclApp app = new JclApp();

    Job job = Job.builder()
        .name("SIMPLE")
        .steps(List.of(
            JclStep.builder().name("STEP01").proc(ProcedureRef.builder().name("PROC01").build()).build()))
        .build();
    app.getJobs().add(job);

    Procedure proc01 = Procedure.builder()
        .name("PROC01")
        .steps(List.of(
            JclStep.builder().name("EXECCBL").pgm(ProgramRef.builder().name("CBL01").build()).build(),
            JclStep.builder().name("EXECEZT").pgm(ProgramRef.builder().name("EZT01").build()).build()))
        .build();
    app.getProcLib().register("PROC01", proc01);

    app.getLinkLib().register("CBL01", ProgramSummary.builder()
        .fileName("CBL01.cbl")
        .programName("CBL01")
        .kind(Program.Kind.COBOL)
        .linesOfCode(4)
        .numberOfConditionals(0)
        .numberOfRoutines(0)
        .build());

    app.getLinkLib().register("EZT01", ProgramSummary.builder()
        .fileName("EZT01.ezt")
        .programName("EZT01")
        .kind(Program.Kind.EASYTRIEVE)
        .linesOfCode(5)
        .numberOfConditionals(0)
        .numberOfRoutines(0)
        .build());

    return app;
  }

  @SuppressWarnings("null")
  @Test
  void resolve_resolvesProcRef_toProcedure() {
    JclApp app = setupSimpleJclApp();

    resolver.resolve(app);

    JclStep step01 = app.getJobs().get(0).getSteps().get(0);
    assertThat(step01.getProc()).isInstanceOf(Procedure.class);
    assertThat(step01.getProc()).isNotInstanceOf(ProcedureRef.class);
    Procedure resolvedProc = (Procedure) step01.getProc();
    assertThat(resolvedProc.getName()).isEqualTo("PROC01");
  }

  @SuppressWarnings("null")
  @Test
  void resolve_resolvesProgramRefs_toPrograms() {
    JclApp app = setupSimpleJclApp();

    resolver.resolve(app);

    Procedure resolvedProc = (Procedure) app.getJobs().get(0).getSteps().get(0).getProc();

    JclStep doThing = resolvedProc.getSteps().get(0);
    assertThat(doThing.getPgm()).isNotNull();
    assertThat(doThing.getPgm()).isInstanceOf(ProgramSummary.class);
    assertThat(((ProgramSummary) doThing.getPgm()).getProgramName()).isEqualTo("CBL01");

    JclStep rptThing = resolvedProc.getSteps().get(1);
    assertThat(rptThing.getPgm()).isNotNull();
    assertThat(rptThing.getPgm()).isInstanceOf(ProgramSummary.class);
    assertThat(((ProgramSummary) rptThing.getPgm()).getProgramName()).isEqualTo("EZT01");
  }

  private JclApp setupJclAppWithSymbolicParameters() {
    JclApp app = new JclApp();

    Job job = Job.builder()
        .name("SYMPARAM")
        .steps(List.of(
            JclStep.builder().name("STEP01").proc(ProcedureRef.builder().name("WITHSYM").build()).build(),
            JclStep.builder().name("STEP02").proc(ProcedureRef.builder().name("WITHSYM").build())
                .symbolicParameters(Map.of(
                    "MBR", "CUSTOM"))
                .build()))
        .build();
    app.getJobs().add(job);

    Procedure withSym = Procedure.builder()
        .name("WITHSYM")
        .symbolicParameterDefaults(Map.of(
            "MBR", "DEFAULT"))
        .steps(List.of(
            JclStep.builder().name("DOIT").pgm(ProgramRef.builder().name("&MBR").build()).build()))
        .build();
    app.getProcLib().register("WITHSYM", withSym);

    app.getLinkLib().register("DEFAULT", ProgramSummary.builder()
        .fileName("DEFAULT.cbl")
        .programName("DEFAULT")
        .kind(Program.Kind.COBOL)
        .linesOfCode(5)
        .numberOfConditionals(0)
        .numberOfRoutines(0)
        .build());

    app.getLinkLib().register("CUSTOM", ProgramSummary.builder()
        .fileName("CUSTOM.cbl")
        .programName("CUSTOM")
        .kind(Program.Kind.COBOL)
        .linesOfCode(9)
        .numberOfConditionals(0)
        .numberOfRoutines(0)
        .build());

    return app;
  }

  @SuppressWarnings("null")
  @Test
  void resolve_respectsDefaultSymbolicParameters() {
    JclApp app = setupJclAppWithSymbolicParameters();

    resolver.resolve(app);

    Procedure resolvedProc = (Procedure) app.getJobs().get(0).getSteps().get(0).getProc();

    JclStep doThing = resolvedProc.getSteps().get(0);
    assertThat(doThing.getPgm()).isNotNull();
    assertThat(doThing.getPgm()).isInstanceOf(ProgramSummary.class);
    assertThat(((ProgramSummary) doThing.getPgm()).getProgramName()).isEqualTo("DEFAULT");
  }

  @SuppressWarnings("null")
  @Test
  void resolve_respectsSymbolicParameterOverrides() {
    JclApp app = setupJclAppWithSymbolicParameters();

    resolver.resolve(app);

    Procedure resolvedProc = (Procedure) app.getJobs().get(0).getSteps().get(1).getProc();

    JclStep stepWithCustomParams = resolvedProc.getSteps().get(1);
    assertThat(stepWithCustomParams.getPgm()).isNotNull();
    assertThat(stepWithCustomParams.getPgm()).isInstanceOf(ProgramSummary.class);
    assertThat(((ProgramSummary) stepWithCustomParams.getPgm()).getProgramName()).isEqualTo("CUSTOM");
  }
}
