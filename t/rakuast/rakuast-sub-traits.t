use Test;

# A sub's `is rw`, `is raw` and `is export` across the RakuAST boundary
# (ADR-10723 Stage 1). Measured on rakudo 2026.09: each is a `Trait::Is` in
# `traits` before `body`; `is export(:a)` carries `argument =>
# Circumfix::Parentheses(…ColonPair::True("a")…)` and several tags an
# `ApplyListInfix(",")` of them. This file passes under both mutsu and raku.

plan 15;

sub routine(Str $src) { $src.AST.statements[0].expression }

{
    my $s = routine(Q[sub f() is export { 1 }]);
    like $s.traits[0].gist, /'from-identifier("export")'/, 'is export is a Trait::Is';
    nok $s.traits[0].gist.contains('argument'), 'a bare is export has no argument';
    like $s.gist, /'traits => (' .* 'body'/, 'before the body';
}

like routine(Q[sub g() is export(:mine) { }]).traits[0].gist,
    /'argument => RakuAST::Circumfix::Parentheses.new(' .* 'ColonPair::True.new("mine")'/,
    'a tag is a ColonPair::True argument';
like routine(Q[sub g() is export(:a, :b) { }]).traits[0].gist,
    /'ApplyListInfix' .* 'ColonPair::True.new("a")' .* 'ColonPair::True.new("b")'/,
    'several tags are a comma list';
like routine(Q[sub h() is rw { }]).traits[0].gist, /'from-identifier("rw")'/, 'is rw';
like routine(Q[sub r() is raw { }]).traits[0].gist, /'from-identifier("raw")'/, 'is raw';

like routine(Q[our sub o() { }]).gist, /'Sub.new(' \s* 'scope => "our",' \s* 'name'/,
    'an our sub leads with scope => "our"';
nok routine(Q[my sub m() { }]).gist.contains('scope'), 'a my sub has no scope field';

# Write direction.
is EVAL(Q[sub f() is export { 41 + 1 }; f()].AST), 42, 'is export round-trips';
is EVAL(Q[sub g() is export(:a, :b) { 7 }; g()].AST), 7, 'tagged is export round-trips';
ok EVAL(Q[sub h($x) is rw { $x }; &h.rw].AST), 'is rw round-trips';
is EVAL(Q[sub r() is raw { 3 }; r()].AST), 3, 'is raw round-trips';
is EVAL(Q[multi sub m(Int) is export { "i" }; multi sub m(Str) is export { "s" }; m(1) ~ m("x")].AST),
    'is', 'exported multi subs round-trip';
is EVAL(Q[package P { our sub o() { 9 } }; P::o()].AST), 9, 'an our sub round-trips into its package';
