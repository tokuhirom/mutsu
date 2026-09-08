use Test;

# An embedded `<?{ … }>` / `<!{ … }>` assertion that assigns to a scalar lexical
# declared OUTSIDE the regex must leave the caller's variable set, exactly as a
# plain `{ … }` block does. The assertion path deliberately does not snapshot the
# env at every cursor position (ADR-0009), so the write is carried through by the
# compiled body's `free_var_writes` instead. See GitHub issue #7593.

plan 9;

my $n = 0;
"aaaa" ~~ / [ <?{ $n++; True }> . ]+ /;
is $n, 5, 'a positive assertion increments the caller scalar';

my $neg = 0;
"aaaa" ~~ / [ <!{ $neg++; False }> . ]+ /;
is $neg, 5, 'a negative assertion increments the caller scalar too';

my $assigned = '';
"abc" ~~ / . <?{ $assigned = 'yes'; True }> . /;
is $assigned, 'yes', 'a plain assignment (not just ++) reaches the caller';

# Still true for the two shapes that already worked, so the new writeback did
# not displace them.
my @a;
"aaaa" ~~ / [ <?{ @a.push(1); True }> . ]+ /;
is @a.elems, 5, 'a container mutated from an assertion still works';

my $c = 0;
"aaaa" ~~ / [ { $c++ } . ]+ /;
is $c, 5, 'a plain code block still writes back';

# The side effect survives a match that ultimately fails.
my $failed = 0;
"aaaa" ~~ / <?{ $failed = 42; True }> b /;
is $failed, 42, 'the write survives a failing match';

# The assertion's own `my` is lexical to it and must NOT reach the caller.
my $x = 'outer';
"ab" ~~ / <?{ my $x = 'inner'; True }> . /;
is $x, 'outer', 'the assertion own my-declaration does not clobber the caller';

# Inside a routine, where the owning slot is the routine's own local.
sub counted() {
    my $k = 0;
    "aaaa" ~~ / [ <?{ $k++; True }> . ]+ /;
    $k;
}
is counted(), 5, 'the write reaches a routine-local slot';

# Inside a grammar, where the body's write compiles to the package-qualified
# name but lands on the bare lexical in env.
my $g = 0;
grammar G {
    token TOP { [ <?{ $g = $g + 1; True }> . ]+ }
}
G.parse("abc");
is $g, 4, 'a grammar token assertion reaches the outer lexical';
