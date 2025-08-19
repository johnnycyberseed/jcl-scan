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
class JclScanUseCaseTests {

  @Autowired
  private AppScanner appScanner;

  @Test
  void scan_writesProgramReportCsvToDisk(@TempDir Path tempDir) throws IOException {
    Path outputDirectory = tempDir.resolve("out");

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
    Path imspgm = pgmsDir.resolve("IMSPGM.cbl");
    Path cobol = pgmsDir.resolve("PAYROLL1.cbl");
    Path ezt = pgmsDir.resolve("EZT1.ezt");

    Files.writeString(job, """
        //DAILY01  JOB
        //STEP01   EXEC DAILYDO
        """.strip(), StandardCharsets.UTF_8);

    Files.writeString(proc, """
        //DAILYDO  PROC
        //*        scenario: using a system library procedure that invokes a program
        //QUERY    EXEC DLIBATCH,MBR=IMSPGM
        //*        scenario: unresolved procedure
        //UNKPROC  EXEC UNKPROC
        //*        scenario: multiple steps in a procedure; referring to COBOL program
        //DOTHING  EXEC PGM=PAYROLL1
        //*        scenario: referring to Easytrieve program
        //RPTTHING EXEC PGM=EZT1
        """.strip(), StandardCharsets.UTF_8);

    Files.writeString(imspgm, """
        IDENTIFICATION DIVISION.
        PROGRAM-ID. IMSPGM.
        PROCEDURE DIVISION.
          DISPLAY 'Load IMS data'.
        STOP RUN.
        """.strip(), StandardCharsets.UTF_8);

    Files.writeString(cobol, """
        IDENTIFICATION DIVISION.
        PROGRAM-ID. PAYROLL1.
        PROCEDURE DIVISION.
          DISPLAY 'Payroll data header'.
          DISPLAY 'Payroll data data'.
          DISPLAY 'Payroll data footer'.
        STOP RUN.
        """.strip(), StandardCharsets.UTF_8);

    Files.writeString(ezt, """
        JOB INPUT
        REPORT REPORT01

          TITLE 'EZT1'
          LINE 01 'Test line'

        END REPORT
        """.strip(), StandardCharsets.UTF_8);

    appScanner.scan(outputDirectory, List.of(root));

    Path programReportFile = outputDirectory.resolve("program-report.csv");

    assertThat(Files.exists(programReportFile)).isTrue();
    String content = Files.readString(programReportFile, StandardCharsets.UTF_8);
    String expected = String.join("\n",
        "File Name,Program Name,Program Type,Lines of Code,Number of conditionals,Number of routines",
        "PAYROLL1.cbl,PAYROLL1,COBOL,7,0,0",
        "IMSPGM.cbl,IMSPGM,COBOL,5,0,0",
        "EZT1.ezt,EZT1,Easytrieve,5,0,0",
        "");
    assertThat(content).isEqualTo(expected);

    Path executionReportFile = outputDirectory.resolve("execution-report.csv");

    assertThat(Files.exists(executionReportFile)).isTrue();
    content = Files.readString(executionReportFile, StandardCharsets.UTF_8);
    expected = String.join("\n",
        "Job,Step,Procedure,Program,Program Type,Lines of Code",
        "DAILY01,STEP01.QUERY.DLIBATCH,(program),IMSPGM,COBOL,5",
        "DAILY01,STEP01.DOTHING,(program),PAYROLL1,COBOL,7",
        "DAILY01,STEP01.RPTTHING,(program),EZT1,EASYTRIEVE,5",
        "");
    assertThat(content).isEqualTo(expected);

    Path unresolvedReportFile = outputDirectory.resolve("unresolved-report.csv");

    assertThat(Files.exists(unresolvedReportFile)).isTrue();
    content = Files.readString(unresolvedReportFile, StandardCharsets.UTF_8);
    expected = String.join("\n",
        "Job,Step,Procedure,Program",
        "DAILY01,STEP01.UNKPROC,UNKPROC,(unknown)",
        "");
    assertThat(content).isEqualTo(expected);
  }
}
