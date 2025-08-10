package com.mechanicalorchard.jclscan.service;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import org.springframework.stereotype.Component;

import com.mechanicalorchard.jclscan.model.ProgramReport;
import com.mechanicalorchard.jclscan.model.ProgramSummary;

@Component
public class CsvReportWriter implements ReportWriter {

    @Override
    public void writeProgramReport(Path outputFile, ProgramReport report) throws IOException {
        try (BufferedWriter writer = Files.newBufferedWriter(outputFile, StandardCharsets.UTF_8)) {
            writer.write("File Name,Program Name,Program Type,Lines of Code,Number of conditionals,Number of routines\n");
            for (ProgramSummary row : report.getRows()) {
                writer.write(String.join(",",
                        escape(row.getFileName()),
                        escape(row.getProgramName()),
                        escape(row.getProgramType()),
                        Integer.toString(row.getLinesOfCode()),
                        Integer.toString(row.getNumberOfConditionals()),
                        Integer.toString(row.getNumberOfRoutines())));
                writer.write("\n");
            }
        }
    }

    private String escape(String value) {
        if (value == null) {
            return "";
        }
        boolean needsQuotes = value.contains(",") || value.contains("\n") || value.contains("\"");
        String escaped = value.replace("\"", "\"\"");
        return needsQuotes ? "\"" + escaped + "\"" : escaped;
    }
}


