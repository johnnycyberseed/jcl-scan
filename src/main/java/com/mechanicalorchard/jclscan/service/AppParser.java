package com.mechanicalorchard.jclscan.service;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mechanicalorchard.jclscan.model.AppSourceFile;
import com.mechanicalorchard.jclscan.model.ProgramSummary;
import com.mechanicalorchard.jclscan.model.JclApp;
import com.mechanicalorchard.jclscan.model.JclScript;
import com.mechanicalorchard.jclscan.model.Job;
import com.mechanicalorchard.jclscan.model.Procedure;
import com.mechanicalorchard.jclscan.model.ProcedureRef;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class AppParser {
  @Autowired
  private JclParser jclParser;
  @Autowired
  private CobolAnalyzer cobolAnalyzer;
  @Autowired
  private EasytrieveAnalyzer easytrieveAnalyzer;

  public JclApp parse(List<AppSourceFile> appSourceFiles, String libraryPrefix) {
    log.info("Parsing {} source files", appSourceFiles.size());
    JclApp app = new JclApp();
    
    // Set library names using the provided prefix
    app.getLinkLib().setName(libraryPrefix + ".LINKLIB");
    app.getProcLib().setName(libraryPrefix + ".PROCLIB");
    
    for (AppSourceFile appSourceFile : appSourceFiles) {
      try {
        String source = appSourceFile.getContent().getContentAsString(Charset.defaultCharset());
        String libMemberName;
        switch(appSourceFile.getKind()) {
          case JCL:
            JclScript jclFile;
            try {
              jclFile = jclParser.parseJclFile(source);
            } catch (IllegalArgumentException e) {
              log.error("Error parsing JCL file {}: {}", appSourceFile.getName(), e.getMessage());
              throw e;
            }
            switch (jclFile) {
              case Job job -> app.getJobs().add(job);
              case Procedure proc -> {
                libMemberName = baseName(appSourceFile.getName());
                app.getProcLib().register(libMemberName, proc);
                log.trace("Registered procedure \"{}\" to \"{}\" in PROC library", proc.getName(), libMemberName);
              }
              case ProcedureRef ref -> throw new IllegalStateException(
                  "Unexpected ProcedureRef: " + ref.getName() + " in " + appSourceFile.getName());
            }
            break;
          case COBOL:
            ProgramSummary cobolSummary = cobolAnalyzer.analyze(appSourceFile.getName(), source, app.getLinkLib().getName());
            libMemberName = baseName(appSourceFile.getName());
            app.getLinkLib().register(libMemberName, cobolSummary);
            log.trace("Registered COBOL program \"{}\" to \"{}\" in LINK library", cobolSummary.getName(), libMemberName);
            break;
          case EASYTRIEVE:
            ProgramSummary easytrieveSummary = easytrieveAnalyzer.analyze(appSourceFile.getName(), source, app.getLinkLib().getName());
            libMemberName = baseName(appSourceFile.getName());
            app.getLinkLib().register(libMemberName, easytrieveSummary);
            log.trace("Registered Easytrieve program \"{}\" to \"{}\" in LINK library", easytrieveSummary.getName(), libMemberName);
            break;
          default:
            break;
        }
      } catch (IOException excp) {
        throw new RuntimeException("Unexpected exception while reading " + appSourceFile.toString(), excp);
      }
    }

    return app;
  }

  private String baseName(String fileName) {
    int dot = fileName.lastIndexOf('.');
    return dot > 0 ? fileName.substring(0, dot) : fileName;
  }
}
