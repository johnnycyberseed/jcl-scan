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

import com.mechanicalorchard.jclscan.model.AppSourceFile;
import com.mechanicalorchard.jclscan.service.SourceDiscoverer;

@SpringBootTest
class SourceDiscovererTests {

  @Autowired
  private SourceDiscoverer sourceDiscoverer;

  @Test
  void discover_buildsAppSourceFilesWithKindAndContent(@TempDir Path tempDir) throws IOException {
    Path job = tempDir.resolve("DAILY01.jcl");
    Path proc = tempDir.resolve("DAILYDO.jcl");
    Path cobol = tempDir.resolve("PAYROLL1.cbl");
    Path ezt = tempDir.resolve("EZT1.ezt");

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

    List<AppSourceFile> discovered = sourceDiscoverer.discover(List.of(job, proc, cobol, ezt));

    assertThat(discovered).hasSize(4);

    // Order should be preserved and kinds should match file extensions
    assertThat(discovered.get(0).getName()).isEqualTo("DAILY01.jcl");
    assertThat(discovered.get(0).getKind()).isEqualTo(AppSourceFile.Kind.JCL);
    assertThat(discovered.get(0).getContent().getContentAsString(StandardCharsets.UTF_8))
        .isEqualTo(Files.readString(job, StandardCharsets.UTF_8));

    assertThat(discovered.get(1).getName()).isEqualTo("DAILYDO.jcl");
    assertThat(discovered.get(1).getKind()).isEqualTo(AppSourceFile.Kind.JCL);
    assertThat(discovered.get(1).getContent().getContentAsString(StandardCharsets.UTF_8))
        .isEqualTo(Files.readString(proc, StandardCharsets.UTF_8));

    assertThat(discovered.get(2).getName()).isEqualTo("PAYROLL1.cbl");
    assertThat(discovered.get(2).getKind()).isEqualTo(AppSourceFile.Kind.COBOL);
    assertThat(discovered.get(2).getContent().getContentAsString(StandardCharsets.UTF_8))
        .isEqualTo(Files.readString(cobol, StandardCharsets.UTF_8));

    assertThat(discovered.get(3).getName()).isEqualTo("EZT1.ezt");
    assertThat(discovered.get(3).getKind()).isEqualTo(AppSourceFile.Kind.EASYTRIEVE);
    assertThat(discovered.get(3).getContent().getContentAsString(StandardCharsets.UTF_8))
        .isEqualTo(Files.readString(ezt, StandardCharsets.UTF_8));
  }
}


