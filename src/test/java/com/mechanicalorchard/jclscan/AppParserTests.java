package com.mechanicalorchard.jclscan;

import java.util.List;
import java.util.Objects;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import com.mechanicalorchard.jclscan.model.AppSourceFile;
import com.mechanicalorchard.jclscan.model.AppSourceFile.Kind;
import com.mechanicalorchard.jclscan.model.ProgramSummary;
import com.mechanicalorchard.jclscan.model.JclApp;
import com.mechanicalorchard.jclscan.model.Procedure;
import com.mechanicalorchard.jclscan.model.Job;
import com.mechanicalorchard.jclscan.model.Program;
import com.mechanicalorchard.jclscan.service.AppParser;
import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
public class AppParserTests {
  @Autowired
  AppParser appParser;

  private JclApp appUnderTest;

  private List<AppSourceFile> sampleAppSourceFiles() {
    return List.of(
        new AppSourceFile("DAILY01", Kind.JCL, """
            //DAILY01  JOB
            //STEP01   EXEC DAILYDO
            """),
        new AppSourceFile("DAILYDO", Kind.JCL, """
            //DAILYDO  PROC
            //DOTHING  EXEC PGM=ACTION01
            //RPTTHING EXEC PGM=REPORT01
            """),
        new AppSourceFile("ACTION01", Kind.COBOL, """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. HELLO-WORLD.
            PROCEDURE DIVISION.
            DISPLAY 'Hello, World!'.
            STOP RUN.
            """),
        new AppSourceFile("REPORT01", Kind.EASYTRIEVE, """
            JOB INPUT
            REPORT TEST-REPORT

              TITLE 'REPORT01'
              LINE 01 'Test line'

            END REPORT
            """)
    );
  }

  @BeforeEach
  void buildSampleApp() {
    appUnderTest = appParser.parse(sampleAppSourceFiles(), "TEST");
  }

  @Test
  void shouldParseEmptyApp() {
    JclApp app = appParser.parse(List.of(), "EMPTY");
    assertThat(app.getJobs()).hasSize(0);
    assertThat(app.getProcLib().size()).isEqualTo(0);
    assertThat(app.getLinkLib().size()).isEqualTo(0);
  }

  @Test
  void shouldPlaceJobsInJobsList() {
    // implicit: only JCL marked as JOBs are treated as jobs (i.e. PROCs are not jobs)
    assertThat(appUnderTest.getJobs()).hasSize(1);
    Job job = appUnderTest.getJobs().get(0);
    assertThat(job.getName()).isEqualTo("DAILY01");
  }

  @Test
  void shouldPlaceProcsInProcLib() {
    assertThat(appUnderTest.getProcLib().size()).isEqualTo(1);
    Procedure proc = (Procedure) appUnderTest.getProcLib().resolve("DAILYDO");

    assertThat(proc.getSteps()).hasSize(2);

    assertThat(proc.getSteps().get(0).getName()).isEqualTo("DOTHING");
    Program pgm = proc.getSteps().get(0).getPgm();
    Objects.requireNonNull(pgm);
    assertThat(pgm.getName()).isEqualTo("ACTION01");

    assertThat(proc.getSteps().get(1).getName()).isEqualTo("RPTTHING");
    pgm = proc.getSteps().get(1).getPgm();
    Objects.requireNonNull(pgm);
    assertThat(pgm.getName()).isEqualTo("REPORT01");
  }

  @Test
  void shouldPlaceProgsInLinkLib() {
    assertThat(appUnderTest.getLinkLib().size()).isEqualTo(2);
    assertThat(appUnderTest.getLinkLib().resolve("ACTION01")).isInstanceOf(ProgramSummary.class);
    assertThat(appUnderTest.getLinkLib().resolve("REPORT01")).isInstanceOf(ProgramSummary.class);
  }
}
