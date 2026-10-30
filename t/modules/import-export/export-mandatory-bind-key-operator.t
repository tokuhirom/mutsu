use v6.e.PREVIEW;
use Test;

# GH-15259 (annotations 0.1.0): operators bound into `EXPORT::MANDATORY` with
# `OUR.WHO.BIND-KEY` at load time are exported and parse as infixes.
plan 2;

use lib 't/lib';
use ExportMandatory;

is 1 mandatory-cat 2, '1+2', 'the bound operator parses and runs as an infix';
is &infix:<mandatory-cat>(3, 4), '3+4', 'and is an imported routine';
