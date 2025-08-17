package com.mechanicalorchard.jclscan;

import com.mechanicalorchard.jclscan.service.JclParser;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import static org.assertj.core.api.Assertions.assertThatThrownBy;


@SpringBootTest
class JclParserEdgeCaseTests {

  @Autowired
  private JclParser jclParser;

  @Test
  void shouldReportTooManyClosingParentheses() {
    String jcl = """
        //PROC1 PROC
        //STEP1 EXEC PGM=MYPGM,COND=(1,LT)),PARAM1=VALUE1,PARAM2=VALUE2
        """;
    assertThatThrownBy(() -> jclParser.parseJclFile(jcl))
        .isInstanceOf(IllegalArgumentException.class)
        .hasMessage("While parsing step STEP1 in \n```jcl\n" + jcl + "```")
        .hasRootCauseInstanceOf(IllegalArgumentException.class)
        .hasRootCauseMessage("Unbalanced parentheses (too many closing?) in parameter list: «PGM=MYPGM,COND=(1,LT)),PARAM1=VALUE1,PARAM2=VALUE2»");
  }

  @Test
  void shouldReportNotEnoughClosingParentheses() {
    String jcl = """
        //PROC1 PROC
        //STEP1 EXEC PGM=MYPGM,COND=(1,LT,PARAM1=VALUE1,PARAM2=VALUE2
        """;
    assertThatThrownBy(() -> jclParser.parseJclFile(jcl))
        .isInstanceOf(IllegalArgumentException.class)
        .hasMessage("While parsing step STEP1 in \n```jcl\n" + jcl + "```")
        .hasRootCauseInstanceOf(IllegalArgumentException.class)
        .hasRootCauseMessage("Unbalanced parentheses (not enough closing?) in parameter list: «PGM=MYPGM,COND=(1,LT,PARAM1=VALUE1,PARAM2=VALUE2»");
  }
}