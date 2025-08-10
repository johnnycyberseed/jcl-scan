# Architecture

## Layers

### Command Line Interface

The CLI is concerned with communicating with the user:
- get user input (in the form of flags and their arguments)
- share progress with the user (via a progress reporter)
- share results with the user

Responsibilities:
1. validate inputs (known flags and well-formed argument values)
2. configure the application with inputs
3. invoke the application

### Application Services

Provide a (high-level) set of services used by the CLI:
- given a set of paths, produce a single List<AppSourceFile>
- given a List<AppSourceFile>, parse into a JclApp
- given a JclApp, produce a ProgramReport
- given a ProgramReport, write it to the output path

### Core Services

Provide a (low-level) set of services used by Application Services:
- given an AppSourceFile of Kind JCL, parse it into a JclFile
- given an AppSourceFile of Kind COBOL, summarize it into a CobolFile
- given an AppSourceFile of Kind Easytrieve, summarize it into a EasytrieveFile
- given an AppSourceFile of Kind Assembler, summarize it into an AssemblerFile
