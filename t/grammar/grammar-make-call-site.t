# A `make` call site remembers that the builtin answered it, so its next
# call skips the general call funnel (#12917). These pin that the site still
# notices everything that can change what `make` means between calls: a
# lexical `make`, a non-Match `$/`, a wrapped routine, and the argument
# shapes the funnel binds. Values are rakudo's.
use Test;

plan 9;

grammar G {
    token TOP { <w>+ % ' ' }
    token w { \w+ }
}

class A {
    method w($/) { make ~$/ ~ '!' }
    method TOP($/) { make $<w>».made.join(',') }
}

{
    my @r = (^3).map({ G.parse('a b c', :actions(A)).made });
    is @r.join(' '), 'a!,b!,c! a!,b!,c! a!,b!,c!', 'repeated parses keep making';
}

{
    my $m = G.parse('x y', :actions(A));
    is $m<w>[1].made, 'y!', 'each node gets its own made value';
}

{
    my @seen;
    for ^3 -> $i {
        'abc' ~~ / b /;
        make "v$i";
        @seen.push: $/.made;
    }
    is @seen.join(','), 'v0,v1,v2', 'a make site called in a loop';
}

{
    my @r;
    for ^3 -> $i {
        if $i < 2 {
            'abc' ~~ / b /;
        }
        else {
            $/ = Nil;
        }
        @r.push: (try { make $i; 'made' }) // 'refused';
    }
    is @r.join(','), 'made,made,refused', 'a non-Match $/ is still rejected on a remembered site';
}

{
    my @r;
    for ^2 {
        'q' ~~ / q /;
        @r.push: make('ok');
    }
    {
        my sub make($v) { "shadowed $v" }
        'q' ~~ / q /;
        @r.push: make('x');
    }
    is @r.join(','), 'ok,ok,shadowed x', 'a lexical make shadows the builtin';
}

{
    my @r;
    for ^3 -> $i {
        'abc' ~~ / b /;
        @r.push: $i == 1 ?? ((try { make(:x(1)); 'bound' }) // 'refused') !! make("m$i");
    }
    is @r.join(','), 'm0,refused,m2', 'a named argument still reaches the binder';
}

{
    'abc' ~~ / b /;
    make('a' => 1) for ^2;
    is $/.made.key, 'a', 'a Pair argument is made as a value';
}

{
    my $calls = 0;
    my sub helper($v) { $v }
    &helper.wrap(-> |c { $calls++; callsame });
    'abc' ~~ / b /;
    make helper('w') for ^2;
    is $calls, 2, 'a wrap elsewhere does not disturb make';
    is $/.made, 'w', 'and make still runs';
}
