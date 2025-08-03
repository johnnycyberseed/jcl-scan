package com.mechanicalorchard.jclscan;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class JclScanApplication {

	public static void main(String[] args) {
		SpringApplication.run(JclScanApplication.class, args);
	}

  /*
   * Sketch
   * 
   * ProcedureLibrary — where procedures are registered
   * LinkLibrary — where programs are registered
   * 
   * Parser:
   * - A JclFile indicates it's either a job or a procedure.
   * - Only procedures are added to the ProcedureLibrary.
   * - So the parse of an application returns:
   *   - a list of jobs (JclFile's)
   *   - the ProcedureLibrary
   *   - the LinkLibrary
   * 
   * - We augment the ProcedureLibrary with built-in procedures (e.g. DLIBATCH, IEBGENER, IDCAMS, etc.)
   * 
   * Resolver:
   * - We put each Job through the resolver:
   *   - resolves procedure references
   *     - the procedure is asked for an effective program (given the LinkLibrary and the JclStep)
   * - at this point, we have a fully scanned application.
   * 
   * Reporters:
   * - Program Report: CSV: Name, Type, LOCs, (maybe # conditionals, # routines)
   * - Execution Report: CSV: Job, Step, Procedure, Program, Program Type, Complexity
   * - Job Report: CSV: Job, Total Complexity
   * 
   * Open Questions:
   * - How will we calculate complexity?
   *   - each procedure can calculate a complexity score
   *     - here, we have shadow procedures for executing programs that do that calculation.
   */
}