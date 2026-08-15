use Test;

# A `rule` (implicit `:sigspace`) inserts a `<.ws>` between adjacent atoms,
# including between the LAST atom and whatever whitespace follows it in the
# pattern source, right up to the closing `}`. The parser used to trim that
# trailing whitespace away (in three places: `parse_raw_braced_regex_body`,
# `normalize_token_pattern`, and `inject_implicit_rule_ws`'s own final
# `.trim()`) before the implicit-`<.ws>` injection pass ever saw it, so
# matching consistently stopped right after the last literal atom.

plan 4;

grammar G1 {
    rule r { 'a' 'b' }
}
{
    my $s = "a b   c";
    my $m = G1.subparse($s, rule => 'r');
    is $m.to, 6, 'rule consumes trailing whitespace after the last atom';
    is $s.substr($m.to), 'c', 'rest of the string starts right after the consumed whitespace';
}

grammar G2 {
    rule perlcapture-begin {
        '<%' 'my' $<name>=<var> '=' 'begin' '%>'
    }
    token var { <sigil> [ \w+ ] }
    token sigil { '&' | '$' }
}
{
    my $s = "<% my &block = begin %>\nHello";
    my $m = G2.subparse($s, rule => 'perlcapture-begin');
    is $m.to, 24, 'trailing newline after the last literal atom is consumed too';
}

# A non-`rule` `token` is unaffected: inter-atom pattern whitespace (including
# a trailing run before the closing `}`) is always pure layout, never a
# matchable atom.
grammar G3 {
    token t { 'a' 'b'   }
}
{
    my $s = "ab   c";
    my $m = G3.subparse($s, rule => 't');
    is $m.to, 2, 'plain token does not gain an implicit trailing <.ws>';
}

done-testing;
