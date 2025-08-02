package com.mechanicalorchard.imogen.jclscan;

import com.mechanicalorchard.imogen.jclscan.model.JclJob;
import com.mechanicalorchard.imogen.jclscan.model.JclStep;
import com.mechanicalorchard.imogen.jclscan.service.JclParserService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
class JclScanApplicationTests {

	@Autowired
	private JclParserService jclParserService;

	@Test
	void contextLoads() {
	}

	@Test
	void shouldParseSimpleJcl() {
		// Given
		String jclContent = """
			//SIMPLE JOB (ACCT),MSGCLASS=H,NOTIFY=&SYSUID
			//STEP1 EXEC PGM=IEFBR14
			""";

		// When
		JclJob jclJob = jclParserService.parse(jclContent);

		// Then
		assertThat(jclJob.getName()).isEqualTo("SIMPLE");
		assertThat(jclJob.getSteps()).hasSize(1);

		JclStep step = jclJob.getSteps().get(0);
		assertThat(step.getName()).isEqualTo("STEP1");
		assertThat(step.getPgm()).isEqualTo("IEFBR14");
		assertThat(step.getProc()).isNull();
	}
}
