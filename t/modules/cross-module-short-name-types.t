use v6;
use lib 't/lib';
use Test;

# Cross-module short-name pollution (the Cro HTTP request-hang root cause):
# bare type names read inside a method body must resolve against the
# DECLARING module's own package chain before another module's short-name
# alias in the global env. The owner-scope fix
# (t/nested-type-short-name-owner-scope.t) pins this within one file; this
# test pins it ACROSS COMPUNITS, which is the shape Cro actually has —
# module loading order is what polluted the one global env.
#
# ShortNameHdr has a class-body lexical `my grammar Header` used by its
# `parse` method; ShortNameRol registers `role Header` inside a nested
# package (this used to unsuppress and overwrite the global short name, so
# `Header.parse` dispatched on the ROLE and died); ShortNameEnu declares
# `my enum Expecting <RequestLine Header Body>` whose member `Header` leaked
# the other way (turning the grammar read into an Int enum value).

use ShortNameHdr;
use ShortNameRol;
use ShortNameEnu;

plan 3;

is ShortNameHdr.parse("abc"), "grammar-ok",
    "class-body lexical grammar wins over another module's role short name";
is ShortNameEnu.check(), "enum-ok",
    "module's own enum member wins over foreign short-name aliases";
is ShortNameHdr.parse("XYZ"), "grammar-fail",
    "the lexical grammar actually runs (non-matching input fails)";
