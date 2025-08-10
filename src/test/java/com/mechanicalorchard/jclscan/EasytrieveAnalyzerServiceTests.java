package com.mechanicalorchard.jclscan;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import com.mechanicalorchard.jclscan.model.Program;
import com.mechanicalorchard.jclscan.model.ProgramSummary;
import com.mechanicalorchard.jclscan.service.EasytrieveAnalyzerService;

@SpringBootTest
class EasytrieveAnalyzerServiceTests {

  @Autowired
  private EasytrieveAnalyzerService easytrieveAnalyzerService;

  @Test
  void contextLoads() {
  }

  @Test
  void shouldAnalyzeEasytrieveFile() {
    // Given
    String easytrieveContent = """
    JOB INPUT
    REPORT HELLO-WORLD

      TITLE 'Hello, World!'
      HEADING 'The Header'
      FOOTING 'The Footer'

      LINE 01 'Line 1'

    END REPORT
    """;

    // When
    ProgramSummary easytrieveFile = easytrieveAnalyzerService.analyze("HELLO.ezt", easytrieveContent);

    // Then
    assertThat(easytrieveFile).isEqualTo(ProgramSummary.builder()
        .programName("HELLO-WORLD")
        .fileName("HELLO.ezt")
        .kind(Program.Kind.EASYTRIEVE)
        .linesOfCode(7)
        .numberOfConditionals(0)
        .numberOfRoutines(0)
        .build());
  }
}
