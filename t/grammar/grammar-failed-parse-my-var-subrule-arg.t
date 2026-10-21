use Test;

plan 7;

# A subrule argument naming a `:my` variable that a code block assigns
# (YAMLish's `<value=map($new-indent)>`). When the parse fails, the diagnostic
# re-walk of the pattern skips the code block, so the variable reads `Nil`
# there; that must not leave a binding error behind for the next `~~`.
my @entered;

grammar G {
    token TOP { <root-block> }
    token root-block {
        :my $new-indent;
        { $new-indent = "" }
        <value=sub-rule($new-indent)>
    }
    token sub-rule(Str $indent) {
        { @entered.push($indent.raku) }
        'k'
    }
}

nok G.parse("\n"), "the parse fails";
is @entered.join(","), '""', "the subrule is entered once, with the assigned value";
ok 5 ~~ Int, "a later smartmatch is not poisoned by a binding error";
ok "abc" ~~ /b/, "nor is a later regex match";

grammar H {
    token TOP { <root-block> }
    token root-block {
        :my $new-indent;
        <?before $<sp>=[' ' ** { 0..* } ] { $new-indent = ~$<sp> }>
        $new-indent
        [ <value=sequence($new-indent)> | <value=map($new-indent)> ]
    }
    token map(Str $indent) { <key>+ % [ \n $indent ] }
    token sequence(Str $indent) { <entry>+ % [ \n $indent ] }
    token entry { '-' }
    token key { 'k' ':' }
}

nok H.parse("\n"), "the nested alternation fails to parse";
ok H.parse("-"), "a valid sequence still parses";
ok 5 ~~ Int, "and the slot stays clean";
