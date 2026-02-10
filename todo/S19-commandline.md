# S19 - Command Line Interface

Reference: `old-design-docs/S19-commandline.pod`

Covers command-line parsing, options, and environment variables.

---

## Command Line Elements

- [x] Script file execution (`mutsu file.p6`)
- [x] `-e` execute code from command line
- [ ] Reading from stdin
- [ ] `@*ARGS` population

---

## Options (Unchanged from Perl 5 Concepts)

- [ ] `-c` check syntax only
- [ ] `-h` help
- [ ] `-I` include paths
- [ ] `-n` awk-like line processing
- [ ] `-p` sed-like line processing
- [ ] `-v` version display
- [ ] `-V` verbose configuration
- [ ] `-S` search PATH for script
- [ ] `-T` taint mode

---

## Option Parsing

- [ ] Long options (`--name`)
- [ ] Short options (`-n`)
- [ ] Single-character clustering (`-abc`)
- [ ] Negation (`/name` or `--/name`)
- [ ] Delimited options (`++NAME ... ++/NAME`)
- [ ] `%*META-ARGS` for subsystem options

---

## MAIN Subroutine Integration

- [x] `MAIN` sub called with parsed arguments
- [ ] Multi dispatch for MAIN variants
- [ ] Signature-based argument binding
- [ ] Auto-generated USAGE from signature
- [ ] `$*USAGE` variable
- [ ] `unit sub MAIN` for file-scoped MAIN

---

## Environment Variables

- [ ] `PERL6OPT` / `RAKULIB` for default options
- [ ] `PERL6LIB` / `RAKULIB` for library paths
- [x] `%*ENV` access to environment
