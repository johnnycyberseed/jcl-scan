package com.mechanicalorchard.jclscan;

import com.mechanicalorchard.jclscan.model.Procedure;
import com.mechanicalorchard.jclscan.model.Job;
import com.mechanicalorchard.jclscan.model.JclScript;
import com.mechanicalorchard.jclscan.model.JclStep;
import com.mechanicalorchard.jclscan.model.ProcedureRef;
import com.mechanicalorchard.jclscan.model.ProgramRef;
import com.mechanicalorchard.jclscan.service.JclParser;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
class JclFileParserTests {

  @Autowired
  private JclParser jclParserService;

  @Test
  void contextLoads() {
  }

  private static Stream<Arguments> jclTestCases() {
    return Stream.of(
        Arguments.of(
            """
                //SIMPLE1 JOB (ACCT),MSGCLASS=H,NOTIFY=&SYSUID
                //* Simple job that calls a COBOL program
                //STEP11 EXEC PGM=MYCBL1
                """,
            Job.builder()
                .name("SIMPLE1")
                .steps(List.of(JclStep.builder()
                    .name("STEP11")
                    .pgm(ProgramRef.builder().name("MYCBL1").build())
                    .proc(null)
                    .build()))
                .build()),
        Arguments.of(
            """
                //SIMPLE2 JOB (ACCT),MSGCLASS=H,NOTIFY=&SYSUID
                //* Simple job that calls a custom procedure
                //* During parsing, we place a reference; presumably we'll resolve it later.
                //STEP21 EXEC PROC=MYPROC
                """,
            Job.builder()
                .name("SIMPLE2")
                .steps(List.of(JclStep.builder()
                    .name("STEP21")
                    .pgm(null)
                    .proc(ProcedureRef.builder().name("MYPROC").build())

                    .build()))
                .build()),
        Arguments.of(
            """
                //PROC1  PROC
                //* Custom procedure that calls a COBOL program
                //STEP31 EXEC PGM=MYCBL3
                """,
            Procedure.builder()
                .name("PROC1")
                .steps(List.of(JclStep.builder()
                    .name("STEP31")
                    .pgm(ProgramRef.builder().name("MYCBL3").build())
                    .proc(null)
                    .build()))
                .build()),
        Arguments.of(
            """
                //PROC2  PROC
                //* Custom procedure that calls a COBOL program
                //STEP41 EXEC MYPROC
                """,
            Procedure.builder()
                .name("PROC2")
                .steps(List.of(JclStep.builder()
                    .name("STEP41")
                    .pgm(null)
                    .proc(ProcedureRef.builder().name("MYPROC").build())

                    .build()))
                .build()));
  }

  @ParameterizedTest
  @MethodSource("jclTestCases")
  void shouldParseJcl(String jclContent, JclScript expectedFile) {
    // When
    JclScript actualFile = jclParserService.parseJclFile(jclContent);

    // Then
    assertThat(actualFile).isEqualTo(expectedFile);
  }

  public static Stream<Arguments> sameJclWithParamsInDifferentOrder() {
    return Stream.of(
        Arguments.of(
            """
                //SIMPLE1 JOB (ACCT),MSGCLASS=H,NOTIFY=&SYSUID
                //* PGM in first position
                //STEP11 EXEC PGM=MYCBL1,COND=(1,LT)
                """,
            Job.builder()
                .name("SIMPLE1")
                .steps(List.of(JclStep.builder()
                    .name("STEP11")
                    .pgm(ProgramRef.builder().name("MYCBL1").build())
                    .proc(null)
                    .build()))
                .build()),
        Arguments.of(
            """
                //SIMPLE1 JOB (ACCT),MSGCLASS=H,NOTIFY=&SYSUID
                //* PGM in second position
                //STEP11 EXEC COND=(1,LT),PGM=MYCBL1
                """,
            Job.builder()
                .name("SIMPLE1")
                .steps(List.of(JclStep.builder()
                    .name("STEP11")
                    .pgm(ProgramRef.builder().name("MYCBL1").build())
                    .proc(null)
                    .build()))
                .build()),
        Arguments.of(
            """
                //SIMPLE1 JOB (ACCT),MSGCLASS=H,NOTIFY=&SYSUID
                //* PGM on a whole different line
                //STEP11 EXEC COND=(1,LT),
                //       PGM=MYCBL1
                """,
            Job.builder()
                .name("SIMPLE1")
                .steps(List.of(JclStep.builder()
                    .name("STEP11")
                    .pgm(ProgramRef.builder().name("MYCBL1").build())
                    .proc(null)
                    .build()))
                .build()));
  }

  @ParameterizedTest
  @MethodSource("sameJclWithParamsInDifferentOrder")
  void shouldFindNamedParamsRegardlessOfOrder(String jclContent, JclScript expectedFile) {
    // When
    JclScript actualFile = jclParserService.parseJclFile(jclContent);

    // Then
    assertThat(actualFile).isEqualTo(expectedFile);
  }

  public static Stream<Arguments> simpleJclWithVariousCombinationsOfParams() {
    return Stream.of(
        Arguments.of(
            """
                //SIMPLE1 JOB (ACCT),MSGCLASS=H,NOTIFY=&SYSUID
                //* Multiple positional parameters
                //STEP11 EXEC PGM=MYPROG,PARAM1=VALUE1,PARAM2=VALUE2,
                //       PARAM3=VALUE3
                """,
            Job.builder()
                .name("SIMPLE1")
                .steps(List.of(JclStep.builder()
                    .name("STEP11")
                    .pgm(ProgramRef.builder().name("MYPROG").build())
                    .proc(null)
                    .symbolicParameters(Map.of(
                        "PARAM1", "VALUE1",
                        "PARAM2", "VALUE2", 
                        "PARAM3", "VALUE3"))
                    .build()))
                .build()),
        Arguments.of(
            """
                //SIMPLE2 PROC
                //* References to symbolic parameters should be copied literally.
                //STEP21 EXEC PGM=MYPROG,PARAM1=&PARAM1,PARAM2=VALUE2,
                //       MBR=&MBR
                """,
            Procedure.builder()
                .name("SIMPLE2")
                .steps(List.of(JclStep.builder()
                    .name("STEP21")
                    .pgm(ProgramRef.builder().name("MYPROG").build())
                    .proc(null)
                    .symbolicParameters(Map.of(
                        "PARAM1", "&PARAM1",
                        "PARAM2", "VALUE2", 
                        "MBR", "&MBR"))
                    .build()))
                .build()),
        Arguments.of(
            """
                //SIMPLE2 PROC
                //* Template placeholders should be copied literally
                //STEP21 EXEC PGM=MYPROG,VALUE1=@VALUE1@,
                //       VALUE2='@VALUE2@'
                """,
            Procedure.builder()
                .name("SIMPLE2")
                .steps(List.of(JclStep.builder()
                    .name("STEP21")
                    .pgm(ProgramRef.builder().name("MYPROG").build())
                    .proc(null)
                    .symbolicParameters(Map.of(
                        "VALUE1", "@VALUE1@",
                        "VALUE2", "'@VALUE2@'"))
                    .build()))
                .build())
                );
  }

  @ParameterizedTest
  @MethodSource("simpleJclWithVariousCombinationsOfParams")
  void shouldStoreAllParams(String jclContent, JclScript expectedFile) {
    // When
    JclScript actualFile = jclParserService.parseJclFile(jclContent);

    // Then
    assertThat(actualFile).isEqualTo(expectedFile);
  }

  @Test
  void shouldReportIsJobWhenIsJob() {
    JclScript actualFile = jclParserService.parseJclFile("""
      //IMAJOB  JOB
      //STEP01  EXEC DUMMY
        """);

    assertThat(actualFile instanceof Job).isTrue();

    actualFile = jclParserService.parseJclFile("""
      //IMAJOB  PROC
      //STEP01  EXEC DUMMY
        """);

    assertThat(actualFile instanceof Job).isFalse();
  }
}
