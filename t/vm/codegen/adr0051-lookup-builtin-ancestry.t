use Test;

# #10132 (ADR-0051 source 12): `.^lookup`/`.^can` scan a builtin type's
# builtin method rows along the chain `.^mro` reports -- ending in Any/Mu,
# never a guessed Cool. Every expectation below is raku-verified (2026-09-29).

plan 17;

# Bootstrap classes whose registry MRO stops at the class itself.
for Promise, Channel, Lock, Supplier, Thread -> $t {
    ok $t.^lookup("gist").defined, "{$t.^name} finds Mu's gist";
}
ok Signature.^lookup("defined").defined, 'Signature finds defined';
ok Promise.^can("Str").elems, 'Promise can Str';

# `Any.list` is an Any method.
ok Any.^lookup("list").defined, 'Any finds list';
ok Date.^lookup("list").defined, 'Date finds list through Any';

# No Cool guess: non-Cool types do not find Cool's methods...
nok Promise.^lookup("uc").defined, 'Promise has no uc';
nok Date.^lookup("chars").defined, 'Date has no chars';
nok Pair.^lookup("abs").defined, 'Pair has no abs';

# ...while genuinely Cool ones do, including a lazily registered IO::Path
# SPEC class.
ok IO::Path.^lookup("chars").defined, 'IO::Path finds chars through Cool';
my $p = IO::Path::Unix.new("x");
ok IO::Path::Unix.^lookup("uc").defined, 'IO::Path::Unix finds uc through Cool';
ok IO::Path::Unix.^lookup("gist").defined, 'IO::Path::Unix finds gist';
ok Instant.^lookup("abs").defined, 'Instant finds abs through Cool';

# Junction skips Any.
nok Junction.^lookup("list").defined, 'Junction does not inherit Any.list';
