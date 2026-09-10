use Test;

plan 8;

# `given`/`with` must rebind `$_` to the topic inside the block, shadowing an
# outer `$_` that already occupies a local slot (a `sub f ($_)` parameter or a
# `my $_`). Regression: the topic was written only to the env mirror, so the
# body read the stale outer `$_` (its slot is authoritative under the (B)
# per-store env-write gate).

sub with_param ($_) { with 42 { return $_ } }
is with_param("outer"), 42, 'with rebinds $_ over a sub $_ parameter';

sub given_param ($_) { given 7 { return $_ } }
is given_param("outer"), 7, 'given rebinds $_ over a sub $_ parameter';

# The rebound topic keeps its real type (not the outer Str).
sub topic_type ($_) {
    my $m = "hello" ~~ /(\w+)/;
    with $m { return $_.WHAT.^name }
}
is topic_type("INPUT"), 'Match', 'rebound topic keeps its Match type';

# `.made` on a rebound Match topic (the vCard::Parser `from-vCard` shape).
grammar G { token TOP { \w+ }; }
class A { method TOP ($/) { make "MADE:$/" } }
sub parse_it ($_) {
    my $r = G.parse($_, actions => A.new);
    with $r { return $_.made }
}
is parse_it("wibble"), 'MADE:wibble', 'with-rebound Match topic supports .made';

# The outer $_ is restored after the block.
sub restored ($_) {
    with 99 { }
    return $_;
}
is restored("KEEP"), 'KEEP', 'outer $_ parameter restored after the block';

# Nested given/with over a sub $_ parameter.
sub nested ($_) {
    given 'a' {
        with 'b' { return "$_" }
    }
}
is nested("x"), 'b', 'nested with over given rebinds correctly';

# The topic is available for smartmatching / method calls, then the outer $_
# parameter is seen again after the block.
sub after ($_) {
    my $seen = do with "abc" { .chars };
    return "$seen:$_";
}
is after("Z"), '3:Z', 'topic method call works, outer $_ seen after';

# No outer $_ (the common case) is unaffected.
sub plain { with 3 { return $_ } }
is plain(), 3, 'with with no outer $_ still works';
