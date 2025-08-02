package com.mechanicalorchard.imogen.jclscan;

import com.mechanicalorchard.imogen.jclscan.model.JclFile;
import com.mechanicalorchard.imogen.jclscan.model.JclStep;
import com.mechanicalorchard.imogen.jclscan.model.ProcRef;
import com.mechanicalorchard.imogen.jclscan.model.ProgRef;
import com.mechanicalorchard.imogen.jclscan.service.JclParserService;
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
class JclParserServiceTests {

  @Autowired
  private JclParserService jclParserService;

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
            JclFile.builder()
                .name("SIMPLE1")
                .steps(List.of(JclStep.builder()
                    .name("STEP11")
                    .pgm(ProgRef.builder().name("MYCBL1").build())
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
            JclFile.builder()
                .name("SIMPLE2")
                .steps(List.of(JclStep.builder()
                    .name("STEP21")
                    .pgm(null)
                    .proc(ProcRef.builder().name("MYPROC").build())

                    .build()))
                .build()),
        Arguments.of(
            """
                //PROC1  PROC
                //* Custom procedure that calls a COBOL program
                //STEP31 EXEC PGM=MYCBL3
                """,
            JclFile.builder()
                .name("PROC1")
                .steps(List.of(JclStep.builder()
                    .name("STEP31")
                    .pgm(ProgRef.builder().name("MYCBL3").build())
                    .proc(null)

                    .build()))
                .build()),
        Arguments.of(
            """
                //PROC2  PROC
                //* Custom procedure that calls a COBOL program
                //STEP41 EXEC MYPROC
                """,
            JclFile.builder()
                .name("PROC2")
                .steps(List.of(JclStep.builder()
                    .name("STEP41")
                    .pgm(null)
                    .proc(ProcRef.builder().name("MYPROC").build())

                    .build()))
                .build()));
  }

  @ParameterizedTest
  @MethodSource("jclTestCases")
  void shouldParseJcl(String jclContent, JclFile expectedFile) {
    // When
    JclFile actualFile = jclParserService.parse(jclContent);

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
            JclFile.builder()
                .name("SIMPLE1")
                .steps(List.of(JclStep.builder()
                    .name("STEP11")
                    .pgm(ProgRef.builder().name("MYCBL1").build())
                    .proc(null)
                    .build()))
                .build()),
        Arguments.of(
            """
                //SIMPLE1 JOB (ACCT),MSGCLASS=H,NOTIFY=&SYSUID
                //* PGM in second position
                //STEP11 EXEC COND=(1,LT),PGM=MYCBL1
                """,
            JclFile.builder()
                .name("SIMPLE1")
                .steps(List.of(JclStep.builder()
                    .name("STEP11")
                    .pgm(ProgRef.builder().name("MYCBL1").build())
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
            JclFile.builder()
                .name("SIMPLE1")
                .steps(List.of(JclStep.builder()
                    .name("STEP11")
                    .pgm(ProgRef.builder().name("MYCBL1").build())
                    .proc(null)
                    .build()))
                .build()));
  }

  @ParameterizedTest
  @MethodSource("sameJclWithParamsInDifferentOrder")
  void shouldFindNamedParamsRegardlessOfOrder(String jclContent, JclFile expectedFile) {
    // When
    JclFile actualFile = jclParserService.parse(jclContent);

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
            JclFile.builder()
                .name("SIMPLE1")
                .steps(List.of(JclStep.builder()
                    .name("STEP11")
                    .pgm(ProgRef.builder().name("MYPROG").build())
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
            JclFile.builder()
                .name("SIMPLE2")
                .steps(List.of(JclStep.builder()
                    .name("STEP21")
                    .pgm(ProgRef.builder().name("MYPROG").build())
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
            JclFile.builder()
                .name("SIMPLE2")
                .steps(List.of(JclStep.builder()
                    .name("STEP21")
                    .pgm(ProgRef.builder().name("MYPROG").build())
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
  void shouldStoreAllParams(String jclContent, JclFile expectedFile) {
    // When
    JclFile actualFile = jclParserService.parse(jclContent);

    // Then
    assertThat(actualFile).isEqualTo(expectedFile);
  }
}
