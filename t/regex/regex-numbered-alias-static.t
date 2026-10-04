use Test;

# Numbered capture aliases (`$N=`) are numbered statically over the whole
# capture level, as rakudo does (#10895): `$N=` sets the counter, a slot filled
# twice or under a quantifier is a list, and slots below N stay unset.

plan 1;

sub check-engine($label) {
    subtest $label => {
        plan 30;

        # An alias inside a group fills the level's slot 0 a second time.
        my $m = "ab" ~~ / (a) [ $0=(b) ] /;
        is $m.list.elems, 1, 'alias in a group refills slot 0';
        isa-ok $m[0], Array, 'a twice-filled slot is a list';
        is $m[0].map(~*).join(','), 'a,b', '...holding both captures in order';

        # An alias inside a quantified group keeps the level's numbering.
        my $o = "x12" ~~ / (x) [ $3=(\d) ]+ /;
        is $o.list.elems, 4, 'quantified group alias numbers from the level';
        is ~$o[0], 'x', 'slot 0 is the leading group';
        nok $o[1].defined, 'slot 1 is unset';
        nok $o[2].defined, 'slot 2 is unset';
        is $o[3].map(~*).join(','), '1,2', 'slot 3 lists every iteration';

        # An alias on a quantified group lists every iteration.
        my $p = "12" ~~ / $0=(\d)+ /;
        is $p.list.elems, 1, 'one slot for an aliased quantified group';
        is $p[0].map(~*).join(','), '1,2', '...listing both digits';

        # A group continues the numbering after an alias.
        my $q = "1a2b" ~~ / [ $1=(\d) (\w) ]+ /;
        is $q.list.elems, 3, 'auto group after an alias takes the next slot';
        nok $q[0].defined, 'slot 0 is unset';
        is $q[1].map(~*).join(','), '1,2', 'slot 1 lists the digits';
        is $q[2].map(~*).join(','), 'a,b', 'slot 2 lists the letters';

        # Two aliases to one slot make a list; the counter moves on after it.
        my $r = "xyb" ~~ / $0=(x) $0=(y) (b) /;
        is $r[0].map(~*).join(','), 'x,y', 'two aliases to $0 make a list';
        is ~$r[1], 'b', 'the following group takes slot 1';

        # Alternation branches start from the same counter; the widest wins.
        my $s = "xb" ~~ / [ (a) | $3=(x) ] (b) /;
        is $s.list.elems, 5, 'alternation continues after its widest branch';
        is ~$s[3], 'x', 'the aliased branch filled slot 3';
        is ~$s[4], 'b', 'the group after the alternation takes slot 4';

        # A group after an alias on a non-capturing group.
        my $t = "ab" ~~ / $1=[a] (b) /;
        is $t.list.elems, 3, 'alias on a non-capturing group sets the counter';
        is ~$t[1], 'a', 'slot 1 is the aliased group';
        is ~$t[2], 'b', 'slot 2 is the following group';

        # An alias deep in a nested capture group numbers that group's level.
        my $u = "abc" ~~ / ( $1=(a) (b) ) (c) /;
        is ~$u[1], 'c', 'the outer level keeps its own numbering';
        is $u[0].list.elems, 3, 'the inner level is numbered on its own';
        is ~$u[0][2], 'b', '...from its alias';

        # Backreferences and code blocks see the static numbers.
        ok "xaa" ~~ / (x) $1=(a) $1 /, 'backreference to an aliased slot';
        nok "xab" ~~ / (x) $1=(a) $1 /, '...which must match the same text';
        my $seen;
        "ab" ~~ / $1=(a) { $seen = ~$1 } (b) /;
        is $seen, 'a', 'a code block reads the aliased slot';

        # Substitution sees the settled slots.
        my $str = "ab";
        $str ~~ s/ $1=(a) (b) /[$1|$2]/;
        is $str, '[a|b]', 'substitution replacement reads the static slots';

        # A grammar rule's level is numbered like a regex's.
        my grammar G {
            token TOP { $1=(\d) (\w) }
        }
        is G.parse("1a").list.map({ .defined ?? ~$_ !! '-' }).join(','), '-,1,a',
            'a grammar rule numbers its level statically';
    }
}

check-engine 'compiled engine';
