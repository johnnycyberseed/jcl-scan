# Features

Primarily JCL Scan is used to quickly survey a given JCL application
in order to seed a spreadsheet to model the complexity of modernizing it.

## Reports to be Generated

There are two "reports" (CSV files generated) JCL Scan produces.

### Program Report

Given a set of program sources (e.g. COBOL, Eeasytrieve, Assembler),
Generate a report that lists:
- File Name
- Program Name (e.g. in COBOL, the PROGRAM-ID. value)
- Program Type (e.g. COBOL, Easytrieve, Assembler)
- Lines of Code
- Number of conditionals (e.g. IF, ELSE, END-IF)
- Number of routines (e.g. SUBROUTINE, FUNCTION)

This report is useful to survey an application for complexity:
- which are the most complex programs?
- what's the distribution of complexity?

### Execution Report

Given a set of JCL sources and the program sources it references,
Generate a report that lists individual executions:
- File Name
- Job Name (symbolic name for the job)
- Step Name 
  - symbolic name for the step
  - if the step is EXECing a proc, the dot-concat of the calling proc to called proc.
- Procedure Name (if the step is EXECing a proc)
- Program Name (if the step is EXECing a program)
- Program Type
- Lines of Code

Think of this as flattening the Job into a sequential list of steps.

This report is useful to enumerate the complexity of each job, step by step:
- when a job is flattened into a list of steps, how many are there?
- considering all the steps of a job, what procedures and programs are invoked?

This provides the raw data to build a spreadsheet to model the complexity of modernizing a job.

## Specifying Source Files

Dimensions:
- identifying source files by path (file, dir, glob)
- indicating a file type (e.g. COBOL, Easytrieve, Assembler)
  - mapping an extension to a type
  - setting a default type for a path (file, dir, glob)

Flags:
- `--source-path <path-to-file | path-to-dir | glob-pattern>`
  - optional config: `...:default-type:<type>;`
- `--file-type <glob:type>`
- `<type>` ::= `cobol` | `easytrieve` | `assembler` | `detect`

## Specifying Output Files

Dimensions:
- location of output files
- format of output files

Flags:
- `--output-path <path-to-dir>`
- `--output-format <format>`
  - `<format>` ::= `csv` (default) | `json`

# Anti-Features

Functionality specifically out of scope for this tool:
- modeling complexity of modernizing a job.
  - additional factors like cyclomatic complexity, novelty, sources of indirection
