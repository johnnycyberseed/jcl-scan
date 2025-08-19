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

    resolver.resolve(app.getJobs(), List.of(app.getProcLib()), List.of(app.getLinkLib()));

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

    resolver.resolve(app.getJobs(), List.of(app.getProcLib()), List.of(app.getLinkLib()));

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
            JclStep.builder().name("WDEFAULT").proc(ProcedureRef.builder().name("WITHSYM").build()).build(),
            JclStep.builder().name("WCUSTOM").proc(ProcedureRef.builder().name("WITHSYM").build())
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
            JclStep.builder().name("IMPLEND").pgm(ProgramRef.builder().name("&MBR").build()).build(),
            JclStep.builder().name("EXPLEND").pgm(ProgramRef.builder().name("&MBR.B").build()).build()
            ))
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

    app.getLinkLib().register("DEFAULTB", ProgramSummary.builder()
        .fileName("DEFAULTB.cbl")
        .programName("DEFAULTB")
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

    app.getLinkLib().register("CUSTOMB", ProgramSummary.builder()
        .fileName("CUSTOMB.cbl")
        .programName("CUSTOMB")
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

    resolver.resolve(app.getJobs(), List.of(app.getProcLib()), List.of(app.getLinkLib()));

    Job symparamJob = app.getJobs().get(0);
    JclStep wdefaultStep = symparamJob.getSteps().get(0);
    Procedure withSymProc01 = (Procedure) wdefaultStep.getProc();

    JclStep implendStep = withSymProc01.getSteps().get(0);
    assertThat(implendStep.getPgm()).isNotNull();
    assertThat(implendStep.getPgm()).isInstanceOf(ProgramSummary.class);
    assertThat(((ProgramSummary) implendStep.getPgm()).getProgramName()).isEqualTo("DEFAULT");

    JclStep explendStep = withSymProc01.getSteps().get(1);
    assertThat(explendStep.getPgm()).isNotNull();
    assertThat(explendStep.getPgm()).isInstanceOf(ProgramSummary.class);
    assertThat(((ProgramSummary) explendStep.getPgm()).getProgramName()).isEqualTo("DEFAULTB");
  }

  @SuppressWarnings("null")
  @Test
  void resolve_respectsSymbolicParameterOverrides() {
    JclApp app = setupJclAppWithSymbolicParameters();

    resolver.resolve(app.getJobs(), List.of(app.getProcLib()), List.of(app.getLinkLib()));

    Job symparamJob = app.getJobs().get(0);
    JclStep wcustomStep = symparamJob.getSteps().get(1);
    Procedure withSymProc02 = (Procedure) wcustomStep.getProc();

    JclStep implendStep = withSymProc02.getSteps().get(0);
    assertThat(implendStep.getPgm()).isNotNull();
    assertThat(implendStep.getPgm()).isInstanceOf(ProgramSummary.class);
    assertThat(((ProgramSummary) implendStep.getPgm()).getProgramName()).isEqualTo("CUSTOM");

    JclStep explendStep = withSymProc02.getSteps().get(1);
    assertThat(explendStep.getPgm()).isNotNull();
    assertThat(explendStep.getPgm()).isInstanceOf(ProgramSummary.class);
    assertThat(((ProgramSummary) explendStep.getPgm()).getProgramName()).isEqualTo("CUSTOMB");
  }

  private JclApp setupJobReferencingProcedureFromSecondaryLibrary() {
    JclApp app = new JclApp();

    Job job = Job.builder()
        .name("WITHSLIB")
        .steps(List.of(
            JclStep.builder().name("STEP01").proc(ProcedureRef.builder().name("DLIBATCH").build()).symbolicParameters(Map.of(
                "MBR", "CBL01",
                "PSB", "CBL01")).build()))
        .build();
    app.getJobs().add(job);

    app.getLinkLib().register("CBL01", ProgramSummary.builder()
        .fileName("CBL01.cbl")
        .programName("CBL01")
        .kind(Program.Kind.COBOL)
        .linesOfCode(4)
        .numberOfConditionals(0)
        .numberOfRoutines(0)
        .build());

    return app;
  }

  private Library<Procedure> setupSecondaryLibrary() {
    Library<Procedure> lib = new Library<>();
    lib.register("DLIBATCH", Procedure.builder()
        .name("DLIBATCH")
        .symbolicParameterDefaults(Map.of(
            "MBR", "",
            "PSB", ""))
        .steps(List.of(JclStep.builder().name("EXECMBR").pgm(ProgramRef.builder().name("&MBR").build()).build()))
        .build());
    return lib;
  }

  @SuppressWarnings("null")
  @Test
  void resolve_resolvesSystemLibraryProcedures() {
    JclApp app = setupJobReferencingProcedureFromSecondaryLibrary();

    List<Library<Procedure>> jclLib = List.of(app.getProcLib(), setupSecondaryLibrary());
    List<Library<Program>> jobLib = List.of(app.getLinkLib());

    resolver.resolve(app.getJobs(), jclLib, jobLib);

    JclStep step01 = app.getJobs().get(0).getSteps().get(0);
    assertThat(step01.getProc()).isInstanceOf(Procedure.class);
    assertThat(step01.getProc()).isNotInstanceOf(ProcedureRef.class);
    Procedure resolvedProc = (Procedure) step01.getProc();
    assertThat(resolvedProc.getName()).isEqualTo("DLIBATCH");

    step01 = resolvedProc.getSteps().get(0);
    assertThat(step01.getPgm()).isNotNull();
    assertThat(step01.getPgm()).isInstanceOf(ProgramSummary.class);
    assertThat(((ProgramSummary) step01.getPgm()).getProgramName()).isEqualTo("CBL01");
  }
}
