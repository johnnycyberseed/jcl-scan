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

@Service
public class AppParser {
  @Autowired
  private JclParser jclParser;
  @Autowired
  private CobolAnalyzer cobolAnalyzer;
  @Autowired
  private EasytrieveAnalyzer easytrieveAnalyzer;

  public JclApp parse(List<AppSourceFile> appSourceFiles) {
    JclApp app = new JclApp();
    
    for (AppSourceFile appSourceFile : appSourceFiles) {
      try {
        String source = appSourceFile.getContent().getContentAsString(Charset.defaultCharset());
        switch(appSourceFile.getKind()) {
          case JCL:
            JclScript jclFile = jclParser.parseJclFile(source);
            switch (jclFile) {
              case Job job -> app.getJobs().add(job);
              case Procedure proc -> app.getProcLib().register(appSourceFile.getName(), proc);
              case ProcedureRef ref -> throw new IllegalStateException(
                  "Unexpected ProcedureRef: " + ref.getName() + " in " + appSourceFile.getName());
            }
            break;
          case COBOL:
            ProgramSummary cobolFile = cobolAnalyzer.analyze(appSourceFile.getName(), source);
            app.getLinkLib().register(appSourceFile.getName(), cobolFile);
            break;
          case EASYTRIEVE:
            ProgramSummary easytrieveFile = easytrieveAnalyzer.analyze(appSourceFile.getName(), source);
            app.getLinkLib().register(appSourceFile.getName(), easytrieveFile);
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
}
