package com.mechanicalorchard.imogen.jclscan;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import com.mechanicalorchard.imogen.jclscan.model.EasytrieveFile;
import com.mechanicalorchard.imogen.jclscan.service.EasytrieveAnalyzerService;

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
    EasytrieveFile easytrieveFile = easytrieveAnalyzerService.analyze("HELLO.ezt", easytrieveContent);

    // Then
    assertThat(easytrieveFile).isEqualTo(EasytrieveFile.builder()
        .name("HELLO-WORLD")
        .id("HELLO.ezt")
        .linesOfCode(7)
        .build());
  }
}
