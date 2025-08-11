package com.mechanicalorchard.jclscan.service;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mechanicalorchard.jclscan.model.AppSourceFile;
import com.mechanicalorchard.jclscan.model.AppSourceFile.Kind;
import com.mechanicalorchard.jclscan.model.JclApp;
import com.mechanicalorchard.jclscan.model.ProgramReport;
import com.mechanicalorchard.jclscan.model.ProgramSummary;

@Service
public class AppScanner {

  @Autowired
  private ReportWriter reportWriter;

  @Autowired
  private ProgramReportBuilder programReportBuilder;

  @Autowired
  private JclAppParserService jclAppParserService;

  public void scan(Path programReportOutputFile) throws IOException {
    // Outside-in anchor: hardcode application sources here
    List<AppSourceFile> sources = List.of(
        new AppSourceFile("DAILY01.jcl", Kind.JCL, """
            //DAILY01  JOB
            //STEP01   EXEC DAILYDO
            """),
        new AppSourceFile("DAILYDO.jcl", Kind.JCL, """
            //DAILYDO  PROC
            //DOTHING  EXEC PGM=PAYROLL1
            //RPTTHING EXEC PGM=EZT1
            """),
        new AppSourceFile("PAYROLL1.cbl", Kind.COBOL, """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. PAYROLL1.
            PROCEDURE DIVISION.
            STOP RUN.
            """),
        new AppSourceFile("EZT1.ezt", Kind.EASYTRIEVE, """
            JOB INPUT
            REPORT EZT1

              TITLE 'EZT1'
              LINE 01 'Test line'

            END REPORT
            """));

    JclApp app = jclAppParserService.parse(sources);

    List<ProgramSummary> summaries = app.getLinkLib().registered().stream()
        .map(p -> (ProgramSummary) p)
        .toList();

    ProgramReport report = programReportBuilder.build(summaries);
    reportWriter.writeProgramReport(programReportOutputFile, report);
  }
}
