package com.mechanicalorchard.jclscan.cli;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

import com.mechanicalorchard.jclscan.model.ProgramSummary;
import com.mechanicalorchard.jclscan.service.AppService;

@Component
@ConditionalOnProperty(name = "jclscan.cli.enabled", havingValue = "true", matchIfMissing = true)
public class CliRunner implements CommandLineRunner {

    private final AppService appService;

    public CliRunner(AppService appService) {
        this.appService = appService;
    }

    @Override
    public void run(String... args) {
        try {
            List<ProgramSummary> summaries = List.of(
                new ProgramSummary("PAYROLL1.cbl", "PAYROLL1", "COBOL", 123, 10, 3),
                new ProgramSummary("EZT1.ezt", "EZT1", "Easytrieve", 200, 8, 5)
            );
            appService.scan(Path.of("program-report.csv"), summaries);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}


