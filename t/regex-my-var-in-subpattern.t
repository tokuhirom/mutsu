use Test;

plan 7;

# An in-regex `:my` lexical is in scope for every sub-pattern of the same regex:
# a group, an alternative, a quantified group, a separated quantifier. Only a
# *subrule* (a different regex) must not see it.
grammar Sub {
    token bare      { 'x' :my $n; <?before $<sp>=' '+ { $n = ~$<sp> }> $n 'a' }
    token group     { 'x' :my $n; <?before $<sp>=' '+ { $n = ~$<sp> }> [ $n 'a' ] }
    token quantified{ 'x' :my $n; <?before $<sp>=' '+ { $n = ~$<sp> }> [ $n 'a' ]+ }
    token separated { 'x' :my $n; <?before $<sp>=' '+ { $n = ~$<sp> }> [ $n 'a' ]+ % \, }
    token alternated{ 'x' :my $n; <?before $<sp>=' '+ { $n = ~$<sp> }> [ 'zz' | $n 'a' ] }
    token captured  { 'x' :my $n; <?before $<sp>=' '+ { $n = ~$<sp> }> ( $n 'a' ) }
    token lines     { 'x' :my $n; <?before $<sp>=' '+ { $n = ~$<sp> }> [ $n $<c>=[\N*] ]+ % \n }
}

ok Sub.subparse('x  a', :rule<bare>), 'a :my lexical interpolates at the top level';
ok Sub.subparse('x  a', :rule<group>), 'and inside a group';
ok Sub.subparse('x  a', :rule<quantified>), 'and inside a quantified group';
ok Sub.subparse('x  a,  a', :rule<separated>), 'and inside a separated quantifier';
ok Sub.subparse('x  a', :rule<alternated>), 'and inside an alternative';
ok Sub.subparse('x  a', :rule<captured>), 'and inside a capturing group';

my $m = Sub.subparse("x  a\n  b", :rule<lines>);
is-deeply $m<c>».Str, ['a', 'b'], 'the measured indent repeats across lines';
