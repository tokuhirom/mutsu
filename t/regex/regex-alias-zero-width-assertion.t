use Test;

# A sigil alias on a `<?name ...>` / `<!name ...>` subrule assertion drops the
# assertion's zero width: Rakudo's `subrule_alias` resets the aliased subrule to
# a capturing call, so `$<a>=<?foo>` is `$<a>=<foo>`. Non-subrule assertions
# (`<?[x]>`) become a plain zero-width subcapture. Issue #9112.

plan 14;

grammar H {
    token foo { x }
    token t1 { $<a>=<foo> }
    token t2 { $<a>=<.foo> }
    token t3 { $<a>=<?foo> x }
    token t3b { $<a>=<?foo> }
    token t4 { $<a>=<?before x> x }
    token t5 { $<a>=<!before y> x }
    token t6 { <?before x> x }
    token t7 { $<a>=<?[x]> x }
    token t8 { $<a>=<!foo> y }
}

sub keys-of($rule) { H.parse('x', :$rule).hash.keys.sort.join(',') }

is keys-of('t1'), 'a,foo', '$<a>=<foo> captures alias and subrule';
is keys-of('t2'), 'a', '$<a>=<.foo> captures only the alias';
nok H.parse('x', :rule<t3>), '$<a>=<?foo> consumes, so a following atom fails';
is H.parse('x', :rule<t3b>).hash.map({ .key ~ '=' ~ .value }).sort.join(','),
    'a=x,foo=x', '$<a>=<?foo> behaves as $<a>=<foo>';
is keys-of('t4'), 'a,before', '$<a>=<?before x> keeps the before key';
is ~H.parse('x', :rule<t4>)<before>, '', 'aliased before capture is zero-width';
nok H.parse('x', :rule<t5>), 'aliased negated lookahead never matches';
nok H.parse('y', :rule<t8>), 'aliased negated subrule never matches';
is keys-of('t6'), '', 'unaliased <?before> captures nothing';
is keys-of('t7'), 'a', 'aliased <?[x]> is a zero-width subcapture';
is ~H.parse('x', :rule<t7>)<a>, '', 'aliased char-class assertion stays zero-width';

my $m = 'ab' ~~ / $<a>=<?alpha> b /;
is ~$m, 'ab', '$<a>=<?alpha> consumes like $<a>=<alpha>';
is $m.hash.keys.sort.join(','), 'a,alpha', 'aliased builtin keeps its own key';
is ('ab' ~~ / <before a> a /).hash.keys.join(','), 'before',
    'bare <before ...> captures before';
