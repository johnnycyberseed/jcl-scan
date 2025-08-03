package com.mechanicalorchard.imogen.jclscan;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import com.mechanicalorchard.imogen.jclscan.model.CobolFile;
import com.mechanicalorchard.imogen.jclscan.service.CobolAnalyzerService;

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
    CobolFile cobolFile = cobolAnalyzerService.analyze("HELLO.cbl", cobolContent);

    // Then
    assertThat(cobolFile).isEqualTo(CobolFile.builder()
        .id("HELLO.cbl")
        .name("HELLO-WORLD")
        .linesOfCode(5)
        .build());
  }
}
