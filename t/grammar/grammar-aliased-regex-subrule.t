use Test;

# `<name=$var>` / `<name={ code }>`: an aliased interpolated Regex is matched as
# an anonymous subrule and filed under the alias only, with its own captures
# nested below it (CSS::Specification's `token val($*EXPR) { <rx={$*EXPR}> }`).

plan 14;

{
    my $r = /<digit>+/;
    my $m = '12' ~~ /<rx=$r>/;
    is ~$m<rx>, '12', '<name=$var> captures under the alias';
    nok $m<$r>:exists, '<name=$var> does not also capture under the variable name';
    is $m<rx><digit>.elems, 2, '<name=$var> nests the regex own captures';

    $m = '12' ~~ /<rx={$r}>/;
    is ~$m<rx>, '12', '<name={ code }> matches the regex the code returns';
    is $m<rx><digit>.elems, 2, '<name={ code }> nests the regex own captures';
}

{
    grammar G {
        token TOP { 'x:' <val(/<digit>+/, 'a number')> }
        token val($*EXPR, $*USAGE = '') { <rx={$*EXPR}> || <usage($*USAGE)> }
        token usage($*USAGE) { \w+ }
    }
    class A {
        method val($/)   { make $<rx> ?? 'rx' !! 'usage' }
        method usage($/) { make ~$*USAGE }
    }
    is G.parse('x:12', :actions(A)).<val>.made, 'rx',
        'a dynamic-parameter regex matches through <rx={$*EXPR}>';
    my $m = G.parse('x:ab', :actions(A));
    is $m<val>.made, 'usage', 'the fallback branch is taken when it does not';
    is $m<val><usage>.made, 'a number', 'the fallback action sees its $* parameter';
}

{
    grammar P1 { token TOP { <v(/\d+/)> }; token v($x) { <$x> } }
    ok P1.parse('12'), '<$x> on a token parameter';
    grammar P2 { token TOP { <v(/\d+/)> }; token v($x) { <foo=$x> } }
    is ~P2.parse('12')<v><foo>, '12', '<name=$x> on a token parameter';
    grammar P3 { token TOP { <v(/\d+/)> }; token v($x) { <foo={$x}> } }
    is ~P3.parse('12')<v><foo>, '12', '<name={$x}> on a token parameter';
}

{
    # A rule inherited from one parent, called with arguments, dispatches its
    # own subrules through the receiver grammar (`Top is Other is Gen`).
    grammar Gen { token cv { \d+ } }
    grammar Other { token val($x) { <cv> } }
    grammar Top is Other is Gen { token TOP { <val(1)> } }
    is ~Top.parse('12')<val><cv>, '12', 'an inherited rule with arguments sees a sibling rule';

    role R { token val($*E) { <rx={$*E}> } }
    grammar Body is Gen does R { }
    grammar Other2 does R { token other { x } }
    grammar Top2 is Other2 is Body { rule TOP { 'c:' <val(/<cv>/)> } }
    my $m = Top2.parse('c: 12');
    ok $m, 'an interpolated regex resolves its subrules in the receiver grammar';
    is ~$m<val><rx><cv>, '12', '... with the nested capture';
}
