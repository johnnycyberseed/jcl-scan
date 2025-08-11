package com.mechanicalorchard.jclscan.cli;

import java.io.IOException;
import java.nio.file.Path;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

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
      appScanner.scan(Path.of("program-report.csv"));
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
}
