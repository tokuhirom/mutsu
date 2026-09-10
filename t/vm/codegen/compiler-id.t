use v6;
use Test;

plan 5;

# `Compiler.id` is callable on the bare type object (it identifies the compiler
# build, not an instance) and returns a defined Str. Rakudo returns the build's
# git SHA; mutsu returns a stable per-build id. Regression: mutsu threw
# "No such method 'id' for invocant of type 'Compiler'" (seen loading the
# Repository::Precomp::Cleanup dist: `my str $id = Compiler.id;`).

my $id = Compiler.id;
isa-ok $id, Str, 'Compiler.id returns a Str';
ok $id.defined, 'Compiler.id is defined';
ok $id.chars > 0, 'Compiler.id is non-empty';

# It is stable across calls (used as a directory-name key).
is Compiler.id, $id, 'Compiler.id is stable across calls';

# The compiler instance reachable via $*RAKU reports the same id.
is $*RAKU.compiler.id, $id, '$*RAKU.compiler.id matches Compiler.id';
