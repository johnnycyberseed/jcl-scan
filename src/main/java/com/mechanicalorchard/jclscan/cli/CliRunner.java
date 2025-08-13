package com.mechanicalorchard.jclscan.cli;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

import com.mechanicalorchard.jclscan.service.AppScanner;

@Component
@ConditionalOnProperty(name = "jclscan.cli.enabled", havingValue = "true", matchIfMissing = true)
public class CliRunner implements CommandLineRunner {

  private final AppScanner appScanner;
  private final ApplicationArguments applicationArguments;

  public CliRunner(AppScanner appScanner, ApplicationArguments applicationArguments) {
    this.appScanner = appScanner;
    this.applicationArguments = applicationArguments;
  }

  @Override
  public void run(String... args) {
    try {
      List<String> sourceValues = applicationArguments.getOptionValues("source-path");
      List<String> outputValues = applicationArguments.getOptionValues("output-path");

      Path outputDirectory = (outputValues == null || outputValues.isEmpty())
          ? Path.of(".")
          : Path.of(outputValues.get(0));

      if (sourceValues == null || sourceValues.isEmpty()) {
        throw new IllegalArgumentException(
            "Usage: jcl-scan --source-path <file|dir> [--source-path <file|dir> ...] [--output-path <dir>]");
      }

      if (!applicationArguments.getNonOptionArgs().isEmpty()) {
        throw new IllegalArgumentException("Unknown positional arguments: " + applicationArguments.getNonOptionArgs());
      }

      List<Path> inputs = sourceValues.stream().map(Path::of).toList();
      appScanner.scan(outputDirectory, inputs);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
}
