package com.mechanicalorchard.jclscan.cli;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

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
      Path output = args.length > 0 ? Path.of(args[0]) : Path.of("program-report.csv");
      List<Path> inputs = new ArrayList<>();
      for (int i = 1; i < args.length; i++) {
        inputs.add(Path.of(args[i]));
      }
      if (!inputs.isEmpty()) {
        appScanner.scan(output, inputs);
      } else {
        throw new IllegalArgumentException("Usage: jcl-scan <output.csv> <input1> <input2> ...");
      }
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
}
