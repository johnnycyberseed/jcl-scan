package com.mechanicalorchard.jclscan;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import com.mechanicalorchard.jclscan.service.AppScanner;
import com.mechanicalorchard.jclscan.model.ProgramSummary;
import com.mechanicalorchard.jclscan.model.Program;
import java.util.List;

@SpringBootTest
class AppServiceIteration1Tests {

  @Autowired
  private AppScanner appScanner;

  @Test
  void scan_writesProgramReportCsvToDisk(@TempDir Path tempDir) throws IOException {
    Path outputFile = tempDir.resolve("program-report.csv");

    List<ProgramSummary> summaries = List.of(
        ProgramSummary
            .builder()
            .fileName("PAYROLL1.cbl")
            .programName("PAYROLL1")
            .kind(Program.Kind.COBOL)
            .linesOfCode(123)
            .numberOfConditionals(10)
            .numberOfRoutines(3)
            .build(),
        ProgramSummary
            .builder()
            .fileName("EZT1.ezt")
            .programName("EZT1")
            .kind(Program.Kind.EASYTRIEVE)
            .linesOfCode(200)
            .numberOfConditionals(8)
            .numberOfRoutines(5)
            .build());

    appScanner.scan(outputFile, summaries);

    assertThat(Files.exists(outputFile)).isTrue();

    String content = Files.readString(outputFile, StandardCharsets.UTF_8).replace("\r\n", "\n");

    String expected = String.join("\n",
        "File Name,Program Name,Program Type,Lines of Code,Number of conditionals,Number of routines",
        "PAYROLL1.cbl,PAYROLL1,COBOL,123,10,3",
        "EZT1.ezt,EZT1,Easytrieve,200,8,5",
        "");

    assertThat(content).isEqualTo(expected);
  }
}
