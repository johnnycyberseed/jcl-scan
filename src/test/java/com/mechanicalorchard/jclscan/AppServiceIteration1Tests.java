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

@SpringBootTest
class AppServiceIteration1Tests {

  @Autowired
  private AppScanner appScanner;

  @Test
  void scan_writesProgramReportCsvToDisk(@TempDir Path tempDir) throws IOException {
    Path outputFile = tempDir.resolve("program-report.csv");

    appScanner.scan(outputFile);

    assertThat(Files.exists(outputFile)).isTrue();

    String content = Files.readString(outputFile, StandardCharsets.UTF_8).replace("\r\n", "\n");

    String expected = String.join("\n",
        "File Name,Program Name,Program Type,Lines of Code,Number of conditionals,Number of routines",
        "PAYROLL1.cbl,PAYROLL1,COBOL,4,0,0",
        "EZT1.ezt,EZT1,Easytrieve,5,0,0",
        "");

    assertThat(content).isEqualTo(expected);
  }
}
