use Test;

plan 8;

# A bare `:my $x;` / `:my $*x;` inside a quantified group is declared once per
# rule invocation: later iterations keep its value (#13255).
grammar G {
    token val { \d+ }
    rule lex  { [ <val> :my $A; <!{ $A++ }> || <val> :my $B; <!{ $B++ }> ]+ }
    rule dyn  { [ <val> :my $*A; <!{ $*A++ }> || <val> :my $*B; <!{ $*B++ }> ]+ }
    token init { [ <val> \s* :my $n = 0; <!{ $n++ }> ]+ }
}
for <lex dyn> -> $r {
    for '1', '1 2', '1 2 3' -> $in {
        my $m = G.subparse($in, :rule($r));
        my $expect = $in eq '1 2 3' ?? 2 !! $in.words.elems;
        is $m<val>.elems, $expect, "$r: '$in' matches $expect value(s)";
    }
}
# An initializer runs on every iteration, so it resets the variable.
is G.subparse('1 2 3', :rule<init>)<val>.elems, 3, 'an initialized :my is re-run per iteration';
is G.subparse('1 2 3', :rule<lex>).Str, '1 2 ', 'the rest is left unmatched';
