package com.mechanicalorchard.jclscan;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
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

  @Test
  void discover_traversesDirectoriesRecursively(@TempDir Path tempDir) throws IOException {
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

    List<AppSourceFile> discovered = sourceDiscoverer.discover(List.of(root));

    assertThat(discovered).hasSize(4);

    // Build a quick lookup by file name (order not guaranteed from directory walk)
    Map<String, AppSourceFile> byName = discovered.stream().collect(java.util.stream.Collectors.toMap(AppSourceFile::getName, x -> x));

    assertThat(byName).containsKeys("DAILY01.jcl", "DAILYDO.jcl", "PAYROLL1.cbl", "EZT1.ezt");

    assertThat(byName.get("DAILY01.jcl").getKind()).isEqualTo(AppSourceFile.Kind.JCL);
    assertThat(byName.get("DAILY01.jcl").getContent().getContentAsString(StandardCharsets.UTF_8))
        .isEqualTo(Files.readString(job, StandardCharsets.UTF_8));

    assertThat(byName.get("DAILYDO.jcl").getKind()).isEqualTo(AppSourceFile.Kind.JCL);
    assertThat(byName.get("DAILYDO.jcl").getContent().getContentAsString(StandardCharsets.UTF_8))
        .isEqualTo(Files.readString(proc, StandardCharsets.UTF_8));

    assertThat(byName.get("PAYROLL1.cbl").getKind()).isEqualTo(AppSourceFile.Kind.COBOL);
    assertThat(byName.get("PAYROLL1.cbl").getContent().getContentAsString(StandardCharsets.UTF_8))
        .isEqualTo(Files.readString(cobol, StandardCharsets.UTF_8));

    assertThat(byName.get("EZT1.ezt").getKind()).isEqualTo(AppSourceFile.Kind.EASYTRIEVE);
    assertThat(byName.get("EZT1.ezt").getContent().getContentAsString(StandardCharsets.UTF_8))
        .isEqualTo(Files.readString(ezt, StandardCharsets.UTF_8));
  }

  @Test
  void discover_findsSourcesFromResources() throws IOException {
    List<AppSourceFile> discovered = sourceDiscoverer.discover(List.of(Paths.get("classpath:libs")));

    assertThat(discovered).hasSize(2);

    assertThat(discovered.get(0).getName()).isEqualTo("IMSBATCH.jcl");
    assertThat(discovered.get(0).getKind()).isEqualTo(AppSourceFile.Kind.JCL);
    assertThat(discovered.get(0).getContent().getContentAsString(StandardCharsets.UTF_8))
        .isEqualTo(readClasspath("libs/sys1/proclib/IMSBATCH.jcl"));

    assertThat(discovered.get(1).getName()).isEqualTo("DLIBATCH.jcl");
    assertThat(discovered.get(1).getKind()).isEqualTo(AppSourceFile.Kind.JCL);
    assertThat(discovered.get(1).getContent().getContentAsString(StandardCharsets.UTF_8))
        .isEqualTo(readClasspath("libs/sys1/proclib/DLIBATCH.jcl"));
  }

  private static String readClasspath(String resourcePath) throws IOException {
    var url = Thread.currentThread().getContextClassLoader().getResource(resourcePath);
    if (url == null) {
      throw new IOException("Classpath resource not found: " + resourcePath);
    }
    try {
      return Files.readString(Path.of(url.toURI()), StandardCharsets.UTF_8);
    } catch (Exception e) {
      throw new IOException("Failed to read classpath resource: " + resourcePath, e);
    }
  }
}


