# TODO Files by Spec Document

Each file tracks implementation status against the corresponding Raku design document (`old-design-docs/Sxx-*.pod`).

## Core Language
| File | Spec | Description |
|------|------|-------------|
| [S02-bits.md](S02-bits.md) | S02 | Types, literals, variables, lexical conventions |
| [S03-operators.md](S03-operators.md) | S03 | Operators, precedence, meta-operators, junctions |
| [S04-control.md](S04-control.md) | S04 | Control flow, phasers, exception handling |
| [S05-regex.md](S05-regex.md) | S05 | Regexes, grammars, longest-token matching |
| [S06-routines.md](S06-routines.md) | S06 | Subroutines, signatures, parameters |

## Data and Collections
| File | Spec | Description |
|------|------|-------------|
| [S07-lists.md](S07-lists.md) | S07 | Lists, Seq, Array, iteration |
| [S08-capture.md](S08-capture.md) | S08 | Capture type, argument binding |
| [S09-data.md](S09-data.md) | S09 | Data structures, typed arrays, hashes |

## Object System
| File | Spec | Description |
|------|------|-------------|
| [S10-packages.md](S10-packages.md) | S10 | Packages, nesting, autoloading |
| [S11-modules.md](S11-modules.md) | S11 | Modules, exportation, versioning |
| [S12-objects.md](S12-objects.md) | S12 | Classes, methods, attributes, inheritance |
| [S13-overloading.md](S13-overloading.md) | S13 | Operator overloading, type casting |
| [S14-roles.md](S14-roles.md) | S14 | Roles, parametric types, traits |

## Specialized
| File | Spec | Description |
|------|------|-------------|
| [S15-unicode.md](S15-unicode.md) | S15 | Unicode, normalization, properties |
| [S16-io.md](S16-io.md) | S16 | I/O, file operations, paths |
| [S17-concurrency.md](S17-concurrency.md) | S17 | Promises, channels, supplies, threads |
| [S19-commandline.md](S19-commandline.md) | S19 | CLI options, MAIN subroutine |
| [S24-testing.md](S24-testing.md) | S24 | Test module, TAP output |
| [S28-special-names.md](S28-special-names.md) | S28 | Special variables, twigils |
| [S29-functions.md](S29-functions.md) | S29 | Built-in functions |

## Not Tracked (out of scope or non-implementation)
| Spec | Reason |
|------|--------|
| S01-overview.pod | Overview only, no implementation items |
| S21-calling-foreign-code.pod | FFI - out of current scope |
| S22-package-format.pod | Package distribution format - out of scope |
| S26-documentation.pod | Pod documentation system - low priority |
| S27-perl-culture-draft.pod6 | Culture document, no implementation items |
| S31-pragmatic-modules.pod | Pragmas - minimal content |
| S99-glossary.pod | Glossary only |
