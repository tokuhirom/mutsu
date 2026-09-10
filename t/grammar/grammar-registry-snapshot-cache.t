use Test;

# Regression test for the regex/grammar registry-snapshot cache
# (perf-regex-registry-snapshot-cache): a grammar-with-actions parse builds a
# sub-interpreter per matched subrule/token and copies the declaration registry
# into it. That copy is now served from a per-interpreter cached snapshot, reused
# by Arc::clone while the registry is unchanged, instead of a full deep clone per
# token match. This guards that:
#   1. grammar+actions still produce correct results (cache preserves semantics),
#   2. repeated parses stay correct (cache reuse across many calls), and
#   3. declaring a NEW class between parses is reflected (cache invalidation).

plan 5;

# Uses a negated SUBRULE (<-sep>) so token resolution copies the registry into a
# sub-interpreter (the path the snapshot cache accelerates).
grammar Words {
    token TOP  { <word>+ % <sep> }
    token word { <-sep>+ }
    token sep  { ' ' }
}
class UpperActions {
    method TOP($/)  { make $<word>.map(*.made).join("-") }
    method word($/) { make $/.Str.uc }
}

is Words.parse("hello world", :actions(UpperActions)).made, "HELLO-WORLD",
    "grammar+actions correct";
is Words.parse("a b c d", :actions(UpperActions)).made, "A-B-C-D",
    "grammar+actions correct (more words)";

# Repeated parses (exercises snapshot cache reuse across many calls).
my @results;
for 1..50 {
    @results.push: Words.parse("foo bar baz", :actions(UpperActions)).made;
}
is @results.elems, 50, "50 repeated parses ran";
is @results.all eq "FOO-BAR-BAZ", True, "every repeated parse correct";

# Declaring more classes (bumps the registry generation -> cache invalidated) must
# not change parse results.
class Extra1 { method x { 1 } }
class Extra2 { method y { 2 } }
is Words.parse("still works", :actions(UpperActions)).made, "STILL-WORKS",
    "parse still correct after new class declarations (cache invalidation)";
