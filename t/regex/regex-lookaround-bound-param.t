use Test;

plan 10;

# A lookaround's body is a sub-pattern of the same regex, so a bound token
# parameter interpolates inside it exactly as it does outside.
grammar Bound {
    token param-outside(Str $indent) { 'x' $indent 'key' }
    token param-inside(Str $indent) { 'x' <?before $indent> 'key' }
    token param-inside-neg(Str $indent) { 'x' <!before $indent> 'zey' }
    token param-inside-seq(Str $indent) { 'x' <?before $indent 'e'> 'key' }
    token param-empty(Str $indent) { 'x' <?before $indent 'k'> 'key' }
    # The keyword may be separated from the body by a newline, not just a space.
    token multiline-keyword(Str $indent) {
        'x'
        <?before
            [ $indent ' '* \n ]*
            $indent $<sp>=' '+
        >
        ' '+ 'key'
    }
    # A `:my` lexical written by a code block inside the lookaround is readable
    # from a quantified group afterwards.
    token measured-indent {
        'x' \n
        :my $ni;
        <?before $<sp>=' '+ { $ni = ~$<sp> }>
        [ $ni $<line>=[\N*] ]+ % \n
    }
}

ok Bound.subparse('xkey', :rule<param-outside>, :args(\(''))), 'bound param outside a lookaround';
ok Bound.subparse('xkey', :rule<param-inside>, :args(\('k'))), 'bound param inside <?before ...>';
ok Bound.subparse('xzey', :rule<param-inside-neg>, :args(\('k'))), 'bound param inside <!before ...>';
ok Bound.subparse('xkey', :rule<param-inside-seq>, :args(\('k'))), 'bound param followed by a literal';
ok Bound.subparse('xkey', :rule<param-inside>, :args(\(''))), 'empty bound param is a zero-width lookahead';
ok Bound.subparse('xkey', :rule<param-empty>, :args(\(''))), 'empty bound param before a literal';
nok Bound.subparse('xkey', :rule<param-inside>, :args(\('q'))), 'a non-matching bound param fails the lookahead';

ok Bound.subparse("x  key", :rule<multiline-keyword>, :args(\(''))),
    'the <?before keyword may be followed by a newline';

my $m = Bound.subparse("x\n  one\n  two", :rule<measured-indent>);
ok $m, 'a :my lexical set inside a lookaround is visible in a later group';
is-deeply $m<line>».Str, ['one', 'two'], 'the group repeated with the measured indent';
