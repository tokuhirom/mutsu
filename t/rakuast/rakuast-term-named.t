use experimental :rakuast;
use Test;

# `RakuAST::Term::Named` (#14916), as rakudo 2026.09: `now` / `time` resolve
# to the setting's `&term:<now>` / `&term:<time>`, a unit's own
# `sub term:<forty-two>` used as `forty-two` is a `Term::Named` too, resolving
# to that `Sub`, and an `EVAL` of the tree calls the term.

plan 10;

sub term:<forty-two> { 42 }

my %seen;
CHECK {
    sub walk($n) {
        if $n ~~ RakuAST::Term::Named {
            %seen{$n.raku.match(/'"' (<-["]>+) '"'/)[0].Str} //=
                [$n.is-resolved, $n.resolution.^name, $n.resolution.compile-time-value];
        }
        $n.visit-children(&walk);
    }
    walk($*CU.statement-list);
}

my @probe = now, time, forty-two;

ok %seen<now>[0], 'now is resolved';
is %seen<now>[1], 'RakuAST::Declaration::External::Setting', 'to the setting';
is %seen<now>[2].name, 'term:<now>', 'its value is the term:<now> routine';
is %seen<time>[2].name, 'term:<time>', 'time likewise';
is %seen<forty-two>[1], 'RakuAST::Sub', "a unit's own term resolves to its Sub";
is %seen<forty-two>[2].(), 42, 'whose value is the routine';

my $ast = Q[sub term:<forty-two> { 42 }; forty-two + 1].AST;
is $ast.statements[1].expression.left.raku, 'RakuAST::Term::Named.new("forty-two")',
    'a user term renders as Term::Named';
is EVAL($ast), 43, 'EVAL of the tree calls the term';
isa-ok EVAL(Q[now].AST), Instant, 'EVAL of now is an Instant';
is @probe[2], 42, 'the probed statement runs';
