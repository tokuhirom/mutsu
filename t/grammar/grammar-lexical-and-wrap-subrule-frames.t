# ADR-0135 Slice E (#7548): a `<&lexical>` call of a lexical Regex runs as a
# frame of the compiled regex engine with the Regex's defining scope in the
# call's binding window, and a `.wrap`ped token only keeps calls of *that*
# token on the walk. A frame enters its callee's ends lazily, so a `{ }` block
# in the callee runs once per end actually entered, as in rakudo; the walk's
# eager producer ran it at every end it computed. Values are rakudo's.
use Test;

plan 6;

{
    my @log;
    my regex lr { a+ { @log.push: $/.to } }
    ok 'aax' ~~ /^ <&lr> 'x'/, 'a lexical regex call matches';
    is @log.join(','), '2', 'its block ran only at the end that was entered';
}

{
    # The callee reads a lexical of the scope it was defined in.
    sub make-rx($want) { my regex inner { (\w+) <?{ ~$0 eq $want }> }; &inner }
    my &picked = make-rx('ab');
    ok 'abc' ~~ /^ <&picked> c/, 'the defining scope is live inside the callee';
}

{
    my @log;
    grammar W {
        regex TOP { <w> <other(2)> 'x' }
        regex w { a }
        regex other($n) { b+ { @log.push: $/.to } }
    }
    W.^find_method('w').wrap(-> |c { callsame });
    ok W.parse('abbx'), 'a grammar with a wrapped token parses';
    is @log.join(','), '3', 'a call of an unwrapped rule is unaffected by the wrap';
}

{
    my @log;
    grammar P {
        regex TOP { <p> 'x' }
        proto regex p {*}
        regex p:sym<a> { a+ { @log.push: $/.to } }
    }
    P.parse('aax');
    is @log.join(','), '2', 'a proto candidate block runs at the entered end only';
}
