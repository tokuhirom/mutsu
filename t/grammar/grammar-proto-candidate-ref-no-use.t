# Source: ASN::META (ASN::Grammar) -- a grammar as the first thing in a unit
# (no `use` statement) referencing a proto candidate as `<value:sym<number>>`
# must not be read as a call to an undeclared routine `sym`.
grammar G {
    proto token value {*}
    token value:sym<number> { \d+ }
    token number { <value:sym<number>> | <[a..z]>+ }
    token TOP { <number> }
}

my $m = G.parse("12");
print "1..3\n";
print $m.defined ?? "ok 1 - parses\n" !! "not ok 1 - parses\n";
print $m<number>.Str eq '12' ?? "ok 2 - captured\n" !! "not ok 2 - captured\n";
print $m<number><value:sym<number>>.defined ?? "ok 3 - candidate captured\n" !! "not ok 3 - candidate captured\n";
