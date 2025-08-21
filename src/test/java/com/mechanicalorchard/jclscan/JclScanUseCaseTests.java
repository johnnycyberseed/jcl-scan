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
        //*        scenario: unresolved program
        //UNKPGM   EXEC PGM=UNKPGM
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
        "Library Name,File Name,Program Name,Program Type,Lines of Code,Number of conditionals,Number of routines",
        "USR1.LINKLIB,PAYROLL1.cbl,PAYROLL1,COBOL,7,0,0",
        "USR1.LINKLIB,IMSPGM.cbl,IMSPGM,COBOL,5,0,0",
        "USR1.LINKLIB,EZT1.ezt,EZT1,Easytrieve,5,0,0",
        "SYS1.LINKLIB,IEBGENER.cbl,IEBGENER,COBOL,156,0,0", // TODO: add column for "library" so that system programs can be differentiated from user programs
        "SYS1.LINKLIB,IDCAMS.cbl,IDCAMS,COBOL,6,0,0",
        "SYS1.LINKLIB,FILEAID.cbl,FILEAID,COBOL,6,0,0",
        "SYS1.LINKLIB,DUMMY.cbl,DUMMY,COBOL,6,0,0",
        "");
    assertThat(content).isEqualTo(expected);

    Path executionReportFile = outputDirectory.resolve("execution-report.csv");

    assertThat(Files.exists(executionReportFile)).isTrue();
    content = Files.readString(executionReportFile, StandardCharsets.UTF_8);
    expected = String.join("\n",
        "Job,Step,Program,Library Name,Program Type,Lines of Code",
        "DAILY01,STEP01.QUERY.DLIBATCH,IMSPGM,USR1.LINKLIB,COBOL,5",
        "DAILY01,STEP01.DOTHING,PAYROLL1,USR1.LINKLIB,COBOL,7",
        "DAILY01,STEP01.RPTTHING,EZT1,USR1.LINKLIB,EASYTRIEVE,5",
        "");
    assertThat(content).isEqualTo(expected);

    Path unresolvedReportFile = outputDirectory.resolve("unresolved-report.csv");

    assertThat(Files.exists(unresolvedReportFile)).isTrue();
    content = Files.readString(unresolvedReportFile, StandardCharsets.UTF_8);
    expected = String.join("\n",
        "Job,Step,Procedure,Program",
        "DAILY01,STEP01.UNKPROC,UNKPROC,(unknown)",
        "DAILY01,STEP01.UNKPGM,(program),UNKPGM",
        "");
    assertThat(content).isEqualTo(expected);
  }

  @Test
  void scan_handlesMultipleSourceLibraries(@TempDir Path tempDir) throws IOException {
    Path outputDirectory = tempDir.resolve("out");

    Path srcsDir = tempDir.resolve("srcs");

    Path src1 = srcsDir.resolve("alpha");
    Files.createDirectories(src1);
    Path src2 = srcsDir.resolve("beta");
    Files.createDirectories(src2);


    Path job1 = src1.resolve("JOB01.jcl");
    Files.writeString(job1, """
        //JOB01    JOB
        //STEP01   EXEC PGM=PGM01
        """.strip(), StandardCharsets.UTF_8);

    Path pgm1 = src1.resolve("PGM01.cbl");
    Files.writeString(pgm1, """
        IDENTIFICATION DIVISION.
        PROGRAM-ID. PGM01.
        PROCEDURE DIVISION.
          DISPLAY 'PGM01'.
        STOP RUN.
        """.strip(), StandardCharsets.UTF_8);

    Path job2 = src2.resolve("JOB02.jcl");
    Files.writeString(job2, """
        //JOB02    JOB
        //STEP01   EXEC PGM=PGM02
        """.strip(), StandardCharsets.UTF_8);

    Path pgm2 = src2.resolve("PGM02.cbl");
    Files.writeString(pgm2, """
        IDENTIFICATION DIVISION.
        PROGRAM-ID. PGM02.
        PROCEDURE DIVISION.
          DISPLAY 'PGM02'.
        STOP RUN.
        """.strip(), StandardCharsets.UTF_8);


    // A library is created for each source directory
    appScanner.scan(outputDirectory, List.of(src1, src2));

    Path programReportFile = outputDirectory.resolve("program-report.csv");

    assertThat(Files.exists(programReportFile)).isTrue();
    String content = Files.readString(programReportFile, StandardCharsets.UTF_8);
    String expected = String.join("\n",
        "Library Name,File Name,Program Name,Program Type,Lines of Code,Number of conditionals,Number of routines",
        "USR1.LINKLIB,PGM01.cbl,PGM01,COBOL,5,0,0",
        "USR2.LINKLIB,PGM02.cbl,PGM02,COBOL,5,0,0",
        "SYS1.LINKLIB,IEBGENER.cbl,IEBGENER,COBOL,156,0,0",
        "SYS1.LINKLIB,IDCAMS.cbl,IDCAMS,COBOL,6,0,0",
        "SYS1.LINKLIB,FILEAID.cbl,FILEAID,COBOL,6,0,0",
        "SYS1.LINKLIB,DUMMY.cbl,DUMMY,COBOL,6,0,0",
        "");
    assertThat(content).isEqualTo(expected);

    Path executionReportFile = outputDirectory.resolve("execution-report.csv");

    assertThat(Files.exists(executionReportFile)).isTrue();
    content = Files.readString(executionReportFile, StandardCharsets.UTF_8);
    expected = String.join("\n",
        "Job,Step,Program,Library Name,Program Type,Lines of Code",
        "JOB01,STEP01,PGM01,USR1.LINKLIB,COBOL,5",
        "JOB02,STEP01,PGM02,USR2.LINKLIB,COBOL,5",
        "");
    assertThat(content).isEqualTo(expected);

    Path unresolvedReportFile = outputDirectory.resolve("unresolved-report.csv");

    assertThat(Files.exists(unresolvedReportFile)).isTrue();
    content = Files.readString(unresolvedReportFile, StandardCharsets.UTF_8);
    expected = String.join("\n",
        "Job,Step,Procedure,Program",
        "");
    assertThat(content).isEqualTo(expected);
  }
}
