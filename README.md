# JCL Scanner

A Java/Spring Boot CLI app that analyzes JCL scripts, producing complexity statistics.

## Quick Start

```bash
./mvnw spring-boot:run -Dspring-boot.run.arguments="--source-path=src/test/resources/examples/simple/ --output-path=temp/output/directory"
```
where:
- `source-path` specifies a filesystem path to a file or directory containing either JCL script or a program listing.
  - can be repeated to specify multiple sources.
  - does not support globbing.
- `output-path` specifies a filesystem path to a directory where the reports will be written.
  - if the directory does not exist, it will be created.

## Overview

**Given** a set of JCL jobs and procedures (aka "JCL App") \
**And** source code for programs invoked by those JCL scripts, \
**Generates** a set of "reports" describing its basic structure and complexity.

### Reports
- Program Report — for each source module (i.e. program), complexity statistics
- Execution Report — for each step within a job (flattening invocations of procedures), the program executed and its complexity statistics

### Complexity statistics:
- Lines of code
- (tbd) count of conditionals
- (tbd) count of subroutines

## Caveats

This tool assumes a lot about the design of the JCL application.

### JCL
- supplied listings are well-formed JCL scripts (i.e. no syntax errors).
- assumes procedures are registered to members using the same name as the PROC symbolic name.
- ignores conditions on execution of steps (e.g. `IF` directives, `COND=` parameters).
- recognizes only cataloged procedures (instream procedures not yet supported).
- only supports simple symbolic parameters (e.g. `&MBR` and `&MBR.B`).

### Programs
- only recognizes COBOL and Easytrieve programs.
- assumes that programs are compiled to a program object / load module of the same name as source member name
  (i.e. the source file within a SRCELIB). \
  _(see https://www.ibm.com/docs/en/zos/2.5.0?topic=introduction-zos-program-management-components)_
- does not support symbolic parameters in program names.

## Troubleshooting

- adjust logging by setting the `logging.level.com.mechanicalorchard.jclscan` property.