use Test;

# From Config::TOML (t/grammar-actions/01-primitives.rakutest): a parse that
# starts at a proto token (`:rule<proto>`) must run the action of the winning
# candidate whichever way it is spelled, not only `:sym<x>`.

plan 3;

grammar G {
    token lit { \d+ }
    proto token s {*}
    token s:plain { <lit> 'p' }
    token s:literal-multi { <lit> 'm' }
}

class A {
    method lit($/) { make(~$/ ~ '!') }
    method s:plain ($/) { make('plain ' ~ $<lit>.made) }
    method s:literal-multi ($/) { make('multi ' ~ $<lit>.made) }
}

is G.parse("12p", :actions(A.new), :rule<s>).made, 'plain 12!', 'bare adverb candidate action runs';
is G.parse("12m", :actions(A.new), :rule<s>).made, 'multi 12!', 'hyphenated bare adverb candidate action runs';

grammar H {
    token lit { \d+ }
    proto token s {*}
    token s:sym<plain> { <lit> 'p' }
}
class B {
    method lit($/) { make(~$/ ~ '?') }
    method s:sym<plain> ($/) { make('sym ' ~ $<lit>.made) }
}
is H.parse("12p", :actions(B.new), :rule<s>).made, 'sym 12?', ':sym<> candidate action still runs';
