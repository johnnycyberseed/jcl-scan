package com.mechanicalorchard.jclscan;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

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

    // Prepare sample inputs on disk in nested directories
    Path root = tempDir.resolve("app");
    Path jobsDir = root.resolve("jobs");
    Path procsDir = root.resolve("procs");
    Path pgmsDir = root.resolve("pgms");
    Files.createDirectories(jobsDir);
    Files.createDirectories(procsDir);
    Files.createDirectories(pgmsDir);

    Path job = jobsDir.resolve("DAILY01.jcl");
    Path proc = procsDir.resolve("DAILYDO.jcl");
    Path cobol = pgmsDir.resolve("PAYROLL1.cbl");
    Path ezt = pgmsDir.resolve("EZT1.ezt");

    Files.writeString(job, """
        //DAILY01  JOB
        //STEP01   EXEC DAILYDO
        """.strip(), StandardCharsets.UTF_8);

    Files.writeString(proc, """
        //DAILYDO  PROC
        //DOTHING  EXEC PGM=PAYROLL1
        //RPTTHING EXEC PGM=EZT1
        """.strip(), StandardCharsets.UTF_8);

    Files.writeString(cobol, """
        IDENTIFICATION DIVISION.
        PROGRAM-ID. PAYROLL1.
        PROCEDURE DIVISION.
        STOP RUN.
        """.strip(), StandardCharsets.UTF_8);

    Files.writeString(ezt, """
        JOB INPUT
        REPORT EZT1

          TITLE 'EZT1'
          LINE 01 'Test line'

        END REPORT
        """.strip(), StandardCharsets.UTF_8);

    appScanner.scan(outputFile, List.of(root));

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
