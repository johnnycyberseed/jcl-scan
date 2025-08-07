package com.mechanicalorchard.jclscan.service;

import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.mechanicalorchard.jclscan.model.AppSourceFile;
import com.mechanicalorchard.jclscan.model.CobolFile;
import com.mechanicalorchard.jclscan.model.EasytrieveFile;
import com.mechanicalorchard.jclscan.model.JclApp;
import com.mechanicalorchard.jclscan.model.JclFile;

@Service
public class JclAppParserService {
  @Autowired
  private JclParserService jclParserService;
  @Autowired
  private CobolAnalyzerService cobolAnalyzerService;
  @Autowired
  private EasytrieveAnalyzerService easytrieveAnalyzerService;

  public JclApp parse(List<AppSourceFile> appSourceFiles) {
    JclApp app = new JclApp();
    
    for (AppSourceFile appSourceFile : appSourceFiles) {
      try {
        String source = appSourceFile.getContent().getContentAsString(Charset.defaultCharset());
        switch(appSourceFile.getKind()) {
          case AppSourceFile.Kind.JCL:
            JclFile jclFile = jclParserService.parseJclFile(source);
            if (jclFile.isJob()) {
              app.getJobs().add(jclFile);
            } else {
              app.getProcLib().register(appSourceFile.getName(), jclFile);
            }
            break;
          case AppSourceFile.Kind.COBOL:
            CobolFile cobolFile = cobolAnalyzerService.analyze(appSourceFile.getName(), source);
            app.getLinkLib().register(appSourceFile.getName(), cobolFile);
            break;
          case AppSourceFile.Kind.EASYTRIEVE:
            EasytrieveFile easytrieveFile = easytrieveAnalyzerService.analyze(appSourceFile.getName(), source);
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
