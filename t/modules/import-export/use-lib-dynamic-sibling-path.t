use v6;
use Test;

plan 3;

# Regression test for
# todo/deep/use-lib-dynamic-path-defers-declaration-visibility-to-parser.md:
#
# `use lib $*PROGRAM.sibling('lib');` -- a non-literal EXPR argument, unlike
# `use lib 'lib';` -- used to be invisible to the PARSER's static `use lib`
# path resolution (only string literals and `$*PROGRAM.parent(N).add(...)`
# chains were recognized). That meant the module it pulled in was never
# scanned for declared subs before the rest of the file was parsed, so a
# later listop-style call with a negative-number first positional argument
# (`matches -5, 'hello'`) misparsed: `matches - 5` was read as subtraction
# and `'hello'` as an unrelated sunk statement, instead of a 2-arg call to
# the imported `multi matches`.
#
# `$*PROGRAM.sibling(...)` is fully resolvable at parse time (the program's
# own path is already known), so it now joins the parser's small family of
# statically-evaluable `use lib` argument shapes alongside literals and
# `.parent(N).add(...)`.

my $exe = $*EXECUTABLE;
my $script = 't/fixtures/use-lib-dynamic-sibling/probe.rakutest';

my $r = run($exe, $script, :out, :err);
my $out = $r.out.slurp(:close).trim;
my $err = $r.err.slurp(:close).trim;

is $r.exitcode, 0, 'a script using `use lib $*PROGRAM.sibling(...)` runs cleanly';
is $out, 'str: -5 hello',
    'the multi sub imported via a dynamic use lib dispatches correctly on a negative first arg';
is $err, '', 'no spurious "Useless use of constant ... in sink context" warning';
