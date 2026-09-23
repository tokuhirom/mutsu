use Test;

plan 14;

# A sigil alias on a *subrule call* followed by a quantifier (`$<a>=<foo>+`)
# is Rakudo's `subrule_alias`: the call itself is renamed `a=foo`, so the
# quantifier repeats a capturing call and `$<a>` is a List with one Match per
# iteration -- exactly what `<foo>+` gives for `$<foo>`. Only a non-subrule
# atom (`$<a>=\w+`, `$<a>=[\w]+`, a char class or property) captures the
# whole quantified span as a single Match. Issue #9128.

grammar H {
    token foo { x }
    token q1 { $<a>=<foo>+ }
    token q2 { $<a>=<?foo>+ }
    token q3 { $<a>=<foo>**2 }
}

my $m = H.parse('xx', :rule<q1>);
is $m<a>.elems, 2, '$<a>=<foo>+ captures one Match per iteration';
is $m<a>.map(*.Str).join('|'), 'x|x', '... each spanning one iteration';
is $m<foo>.elems, 2, 'the subrule name is published per iteration too';

is H.parse('xx', :rule<q2>)<a>.elems, 2, '$<a>=<?foo>+ is the same as $<a>=<foo>+';
is H.parse('xx', :rule<q3>)<a>.elems, 2, '$<a>=<foo>**2 captures both iterations';

# Builtin subrules are subrule calls too, captured or dot-suppressed.
is ('xx' ~~ /$<a>=<alpha>+/)<a>.elems, 2, '$<a>=<alpha>+ is a List';
is ('xx' ~~ /$<a>=<.alpha>+/)<a>.elems, 2, '$<a>=<.alpha>+ is a List';
is ('xx' ~~ /$<a>=<.alpha>**2/)<a>.elems, 2, '$<a>=<.alpha>**2 is a List';
is ('xx' ~~ /@<a>=<alpha>+/)<a>.elems, 2, '@<a>=<alpha>+ is a List';
is-deeply ('' ~~ /$<a>=<alpha>*/)<a>, [], 'zero iterations give an empty List';

# Non-subrule atoms still capture the whole span as one Match.
is ~('xx' ~~ /$<a>=\w+/)<a>, 'xx', '$<a>=\w+ captures the span';
is ~('xx' ~~ /$<a>=[\w]+/)<a>, 'xx', '$<a>=[\w]+ captures the span';
is ~('xx' ~~ /$<a>=<[a..z]>+/)<a>, 'xx', '$<a>=<[a..z]>+ captures the span';
is ~('xx' ~~ /$<a>=<:L>+/)<a>, 'xx', '$<a>=<:L>+ captures the span';
