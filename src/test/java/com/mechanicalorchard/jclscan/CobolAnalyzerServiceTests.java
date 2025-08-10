package com.mechanicalorchard.jclscan;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import com.mechanicalorchard.jclscan.model.Program;
import com.mechanicalorchard.jclscan.model.ProgramSummary;
import com.mechanicalorchard.jclscan.service.CobolAnalyzerService;

@SpringBootTest
class CobolAnalyzerServiceTests {

  @Autowired
  private CobolAnalyzerService cobolAnalyzerService;

  @Test
  void contextLoads() {
  }

  @Test
  void shouldAnalyzeCobolFile() {
    // Given
    String cobolContent = """
    IDENTIFICATION DIVISION.
    PROGRAM-ID. HELLO-WORLD.
    PROCEDURE DIVISION.
    DISPLAY 'Hello, World!'.
    STOP RUN.
    """;

    // When
    ProgramSummary cobolFile = cobolAnalyzerService.analyze("HELLO.cbl", cobolContent);

    // Then
    assertThat(cobolFile).isEqualTo(ProgramSummary.builder()
        .fileName("HELLO.cbl")
        .programName("HELLO-WORLD")
        .kind(Program.Kind.COBOL)
        .linesOfCode(5)
        .numberOfConditionals(0)
        .numberOfRoutines(0)
        .build());
  }
}
