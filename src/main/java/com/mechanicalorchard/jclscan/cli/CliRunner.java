package com.mechanicalorchard.jclscan.cli;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

import com.mechanicalorchard.jclscan.model.Program;
import com.mechanicalorchard.jclscan.model.ProgramSummary;
import com.mechanicalorchard.jclscan.service.AppScanner;

@Component
@ConditionalOnProperty(name = "jclscan.cli.enabled", havingValue = "true", matchIfMissing = true)
public class CliRunner implements CommandLineRunner {

  private final AppScanner appScanner;

  public CliRunner(AppScanner appScanner) {
    this.appScanner = appScanner;
  }

  @Override
  public void run(String... args) {
    try {
      List<ProgramSummary> summaries = List.of(
          ProgramSummary.builder()
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
      appScanner.scan(Path.of("program-report.csv"), summaries);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
}
