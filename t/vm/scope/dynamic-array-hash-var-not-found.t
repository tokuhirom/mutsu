use Test;

plan 7;

# A never-declared `@*`/`%*`-twigil (dynamic) variable is `X::Dynamic::NotFound`
# territory in real Raku: the caller-chain lookup finds nothing, so the read
# comes back undefined, not an auto-vivified empty container. mutsu used to
# treat `@*name`/`%*name` the same as a plain undeclared `@name`/`%name`
# (auto-vivified to a defined `[]`/`{}`), which broke `//` fallback idioms —
# found via App::ShowPath's dependency chain (Test::META.rakumod):
#
#     sub meta-candidates() {
#         @*META-CANDIDATES // <META6.json META.info>;
#     }
#
# never reached its fallback list because `@*META-CANDIDATES` read back as a
# defined (if empty) Array, not Nil.

sub arr-check() {
    @*UNDECLARED-ARR.defined;
}
sub hash-check() {
    %*UNDECLARED-HASH.defined;
}

nok arr-check(), 'a never-declared @*-twigil var reads as undefined';
nok hash-check(), 'a never-declared %*-twigil var reads as undefined';

sub arr-fallback() {
    @*UNDECLARED-ARR // <a b c>;
}
sub hash-fallback() {
    %*UNDECLARED-HASH // { x => 1 };
}

is arr-fallback(), <a b c>, '// falls through to its RHS for @* not found';
is hash-fallback(), { x => 1 }, '// falls through to its RHS for %* not found';

# A DECLARED dynamic array/hash is unaffected -- it is found before the
# undeclared fallback and keeps its own value (including a legitimately
# empty one).
sub arr-declared() {
    my @*d = 1, 2, 3;
    inner-arr-declared();
}
sub inner-arr-declared() {
    @*d;
}
is arr-declared(), (1, 2, 3), 'a declared @*-twigil var is read normally';

sub hash-declared() {
    my %*d = a => 1;
    inner-hash-declared();
}
sub inner-hash-declared() {
    %*d;
}
is hash-declared(), { a => 1 }, 'a declared %*-twigil var is read normally';

# A legitimately declared but empty dynamic array/hash still reads back
# defined -- the fix must not turn every empty container into Nil, only an
# actually-never-declared one.
sub arr-declared-empty() {
    my @*e;
    inner-arr-declared-empty();
}
sub inner-arr-declared-empty() {
    @*e.defined;
}
ok arr-declared-empty(), 'a declared-but-empty @*-twigil var is still defined';

# vim: expandtab shiftwidth=4
