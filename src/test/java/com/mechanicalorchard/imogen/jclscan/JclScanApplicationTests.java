package com.mechanicalorchard.imogen.jclscan;

import com.mechanicalorchard.imogen.jclscan.model.JclFile;
import com.mechanicalorchard.imogen.jclscan.model.JclStep;
import com.mechanicalorchard.imogen.jclscan.model.ProcRef;
import com.mechanicalorchard.imogen.jclscan.model.ProgRef;
import com.mechanicalorchard.imogen.jclscan.service.JclParserService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
class JclScanApplicationTests {

	@Autowired
	private JclParserService jclParserService;

	@Test
	void contextLoads() {
	}

	private static Stream<Arguments> jclTestCases() {
		return Stream.of(
			Arguments.of(
				"""
				//SIMPLE1 JOB (ACCT),MSGCLASS=H,NOTIFY=&SYSUID
				//* Simple job that calls a COBOL program
				//STEP11 EXEC PGM=MYCBL1
				""",
				JclFile.builder()
					.name("SIMPLE1")
					.steps(List.of(JclStep.builder()
						.name("STEP11")
						.pgm(ProgRef.builder().name("MYCBL1").build())
						.proc(null)
						.build()))
					.build()
			),
			Arguments.of(
				"""
				//SIMPLE2 JOB (ACCT),MSGCLASS=H,NOTIFY=&SYSUID
				//* Simple job that calls a custom procedure
				//* During parsing, we place a reference; presumably we'll resolve it later.
				//STEP21 EXEC PROC=MYPROC
				""",
				JclFile.builder()
					.name("SIMPLE2")
					.steps(List.of(JclStep.builder()
						.name("STEP21")
						.pgm(null)
						.proc(ProcRef.builder().name("MYPROC").build())
						.build()))
					.build()
			),
			Arguments.of(
				"""
				//PROC1  PROC
				//* Custom procedure that calls a COBOL program
				//STEP31 EXEC PGM=MYCBL3
				""",
				JclFile.builder()
					.name("PROC1")
					.steps(List.of(JclStep.builder()
						.name("STEP31")
						.pgm(ProgRef.builder().name("MYCBL3").build())
						.proc(null)
						.build()))
					.build()
			)
		);
	}

	@ParameterizedTest
	@MethodSource("jclTestCases")
	void shouldParseJcl(String jclContent, JclFile expectedFile) {
		// When
		JclFile actualFile = jclParserService.parse(jclContent);

		// Then
		assertThat(actualFile).isEqualTo(expectedFile);
	}

}
