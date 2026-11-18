# A slang-activated hook module's MANDATORY operators reach the importer's parse

A module that imports through a `sub EXPORT` hook which may bind
`LEXICAL::<EXPORTHOW>` (annotations: `use annotations <declare direct class>`
gives the importer an `annotation` declarator) is activated for the importer's
parse in the slang activation interpreter, until ADR-17558 slice 4 loads such
modules in the program interpreter. That activation reported the declarators
and grammar rules the module added, but not the routines it exports, so an
operator the module binds into `EXPORT::MANDATORY` at run time (annotations:
`OUR.WHO.BIND-KEY: .key, .value for annotations::core::`) was unknown to the
parser: `$?CLASS graffiti values` was "Two terms in a row".

The activation now also reports the routines the module exports to the `use`,
with their operator precedence, and the parser registers them as it does for
a module loaded at parse time. annotations' `t/02-direct.rakutest` passes again
(#18091).
