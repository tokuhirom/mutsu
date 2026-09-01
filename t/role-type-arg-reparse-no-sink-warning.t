use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 4;

# Instantiating a parametric role re-parses its type-argument source (`Int` of
# `R1[Int]`) at run time. That fragment is not a compilation unit, so it must
# not be run through the mainline sink-context analysis -- it used to report a
# spurious `Useless use of constant value Int in sink context`.
is_run 'my role R1[::T] { }; my R1 of Int $x = R1[Int].new; say "ok"',
    { :0status, :out("ok\n"), :err('') },
    'role type argument re-parse raises no sink warning';

is_run 'EVAL q|my role R1[::T] { }; my R1 of Int $x = R1[Int].new;|; say "ok"',
    { :0status, :out("ok\n"), :err('') },
    'role type argument re-parse inside EVAL raises no sink warning';

# The fragment re-parse also used to *clear* the enclosing unit's collected
# warnings, so a genuine one raised before it was silently dropped.
is_run 'EVAL q|my role R1[::T] { }; 42; my R1 of Int $x = R1[Int].new; 1|; say "ok"',
    { :0status, :out("ok\n"), :err(/"Useless use of constant integer 42 in sink context"/) },
    'a real sink warning survives a later role type argument re-parse';

# A later lexical role of the same short name keeps its own methods.
is_run 'try EVAL q|my role R1[::T] { }; my R1 of Str $x = R1[Int].new;|;
        { my role R1[::T] { method x { T } }; say R1[Int].new.x.^name }',
    { :0status, :out("Int\n") },
    'EVAL-declared role does not shadow a later lexical role of the same name';
