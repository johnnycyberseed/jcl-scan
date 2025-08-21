package com.mechanicalorchard.jclscan;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import com.mechanicalorchard.jclscan.service.AppScanner;

@SpringBootTest
public class JclScanIncludedProcsAndProgsTests {

  @Autowired
  private AppScanner appScanner;

  @Test
  void scan_includesSystemProcsAndProgs(@TempDir Path tempDir) throws IOException {
    Path outputDirectory = tempDir.resolve("out");

    // Prepare sample inputs on disk in nested directories
    Path root = tempDir.resolve("app");

    Path srcDir = root.resolve("src");

    Files.createDirectories(srcDir);

    Path job = srcDir.resolve("JOB.jcl");

    Files.writeString(job, """
        //JOB  JOB (0000000000,000000,00000000,0000000,00000)
        //*
        //* Included procedures
        //COBUCL   EXEC COBUCL
        //DLIBATCH EXEC DLIBATCH,MBR=DUMMY
        //FTPBATCH EXEC FTPBATCH
        //IMSBATCH EXEC IMSBATCH,MBR=DUMMY
        //IMSCOC   EXEC IMSCOC
        //IMSCON   EXEC IMSCON
        //UCC11RMS EXEC UCC11RMS
        //UDRBATCH EXEC UDRBATCH
        //ULUBATCH EXEC ULUBATCH
        //*
        //* Included programs
        //FILEAID  EXEC PGM=FILEAID
        //IDCAMS   EXEC PGM=IDCAMS
        //IEBGENER EXEC PGM=IEBGENER
        //*
        """.strip(), StandardCharsets.UTF_8);

    appScanner.scan(outputDirectory, List.of(root));

    Path programReportFile = outputDirectory.resolve("program-report.csv");

    assertThat(Files.exists(programReportFile)).isTrue();
    String content = Files.readString(programReportFile, StandardCharsets.UTF_8);
    String expected = String.join("\n",
        "Library Name,File Name,Program Name,Program Type,Lines of Code,Number of conditionals,Number of routines",
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
        "Job,Step,Procedure,Program,Program Type,Lines of Code",
        "JOB,COBUCL.DUMMY,(program),DUMMY,COBOL,6",
        "JOB,DLIBATCH.DLIBATCH,(program),DUMMY,COBOL,6",
        "JOB,FTPBATCH.DUMMY,(program),DUMMY,COBOL,6",
        "JOB,IMSBATCH.IMSBATCH,(program),DUMMY,COBOL,6",
        "JOB,IMSCOC.DUMMY,(program),DUMMY,COBOL,6",
        "JOB,IMSCON.DUMMY,(program),DUMMY,COBOL,6",
        "JOB,UCC11RMS.DUMMY,(program),DUMMY,COBOL,6",
        "JOB,UDRBATCH.DUMMY,(program),DUMMY,COBOL,6",
        "JOB,ULUBATCH.DUMMY,(program),DUMMY,COBOL,6",
        "JOB,FILEAID,(program),FILEAID,COBOL,6",
        "JOB,IDCAMS,(program),IDCAMS,COBOL,6",
        "JOB,IEBGENER,(program),IEBGENER,COBOL,156",
        "");
    assertThat(content).isEqualTo(expected);

    Path unresolvedReportFile = outputDirectory.resolve("unresolved-report.csv");

    // ... with no unresolved references
    assertThat(Files.exists(unresolvedReportFile)).isTrue();
    content = Files.readString(unresolvedReportFile, StandardCharsets.UTF_8);
    expected = String.join("\n",
        "Job,Step,Procedure,Program",
        "");
    assertThat(content).isEqualTo(expected);
  }
}
