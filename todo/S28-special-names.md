# S28 - Special Names

Reference: `old-design-docs/S28-special-names.pod`

Covers special variables, twigils, and Perl 5 to Perl 6 translation.

---

## Twigil System

- [x] `$?foo` compile-time constants
- [x] `$*foo` dynamic/global variables
- [ ] `$=foo` Pod-scoped variables

---

## Compile-time Variables ($?)

- [x] `$?FILE` - current filename
- [x] `$?LINE` - current line number
- [ ] `$?PACKAGE` - current package
- [ ] `$?MODULE` - current module
- [ ] `$?CLASS` - current class
- [ ] `$?ROLE` - current role
- [ ] `$?GRAMMAR` - current grammar
- [ ] `$?TABSTOP` - tab stop width (default 8)
- [ ] `$?NF` - Unicode normalization form
- [ ] `$?DISTRIBUTION` - current distribution

---

## Dynamic Variables ($*)

### Process Information
- [x] `$*PID` - process ID
- [x] `$*PROGRAM` - program name
- [ ] `$*PROGRAM-NAME` - program name (alias)
- [ ] `$*EXECUTABLE` - path to interpreter
- [ ] `$*USER` - current user
- [ ] `$*GROUP` - current group

### I/O
- [x] `$*OUT` - standard output (implicit)
- [x] `$*ERR` - standard error (implicit)
- [ ] `$*IN` - standard input
- [ ] `$*ARGFILES` - magic command-line file handle

### Environment
- [x] `%*ENV` - environment variables
- [x] `@*ARGS` - command-line arguments (partial)
- [x] `$*CWD` - current working directory

### System Information
- [ ] `$*KERNEL` - kernel information (name, release, hardware)
- [ ] `$*DISTRO` - distribution information
- [ ] `$*VM` - virtual machine metadata
- [ ] `$*PERL` / `$*RAKU` - language version info

### Scheduler
- [ ] `$*SCHEDULER` - default task scheduler
- [ ] `$*THREAD` - current thread

---

## Match Variables

- [x] `$/` - current match object
- [x] `$0`, `$1`, ... - positional captures
- [x] `$<name>` - named captures
- [x] `$!` - current exception

---

## Perl 5 to Perl 6 Variable Mapping

| Perl 5 | Perl 6 | Status |
|--------|--------|--------|
| `$_` | `$_` | [x] |
| `@_` | `@_` (placeholder) | [x] |
| `$0` | `$*PROGRAM-NAME` | [ ] |
| `$/` | `$*IN.input-line-separator` | [ ] |
| `$\` | `$*OUT.output-line-separator` | [ ] |
| `$,` | `$*OUT.output-field-separator` | [ ] |
| `$$` | `$*PID` | [x] |
| `$!` | `$!` (exception) | [x] |
| `@ARGV` | `@*ARGS` | [x] (partial) |
| `%ENV` | `%*ENV` | [x] |
| `$ARGV` | `$*ARGFILES.path` | [ ] |
| `STDIN` | `$*IN` | [ ] |
| `STDOUT` | `$*OUT` | [x] (implicit) |
| `STDERR` | `$*ERR` | [x] (implicit) |
