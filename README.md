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
- does not respect `IF` directives in JCL scripts
- does not include the execution of user-authored programs when invoked indirectly by procedures (e.g. `DLIBATCH`).
- assumes that procedures are registered to members using the same name as the PROC symbolic name.

### Programs
- only recognizes COBOL and Easytrieve programs.
- assumes that programs are compiled to a program object / load module of the same name as the program id.
  (i.e. the program's logical identifer). \
  _(see https://www.ibm.com/docs/en/zos/2.5.0?topic=introduction-zos-program-management-components)_

## Troubleshooting

- adjust logging by setting the `logging.level.com.mechanicalorchard.jclscan` property.