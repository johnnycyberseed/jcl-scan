package com.mechanicalorchard.jclscan;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import com.mechanicalorchard.jclscan.model.AppSourceFile;
import com.mechanicalorchard.jclscan.model.AppSourceFile.Kind;
import com.mechanicalorchard.jclscan.model.JclApp;
import com.mechanicalorchard.jclscan.service.JclAppParserService;
import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
public class JclAppParserTests {
  @Autowired
  JclAppParserService jclAppParserService;

  @Test
  void shouldParseJclApp() {
    List<AppSourceFile> appSourceFiles = List.of(
        new AppSourceFile("DAILY01", Kind.JCL, """
            //DAILY01  JOB
            //STEP01   EXEC DAILYDO
            """),
        new AppSourceFile("DAILYDO", Kind.JCL, """
            //DAILYDO  PROC
            //DOTHING  EXEC PGM=ACTION01
            """),
        new AppSourceFile("ACTION01", Kind.COBOL, """
            IDENTIFICATION DIVISION.
            PROGRAM-ID. HELLO-WORLD.
            PROCEDURE DIVISION.
            DISPLAY 'Hello, World!'.
            STOP RUN.
            """)
    );

    JclApp app = jclAppParserService.parse(appSourceFiles);
    assertThat(app.getJobs()).size().isEqualTo(1);
    assertThat(app.getProcLib().size()).isEqualTo(1);
    assertThat(app.getLinkLib().size()).isEqualTo(1);
  }
}
