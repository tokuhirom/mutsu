use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: an anonymous (`subset ::`)
# or inline (`(subset T of Int)`) subset places its `Name` over the name or
# `::`, and its `Trait::Of` over `of TYPE`, as a named subset statement does.

plan 3;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) if $class ~~ /Name|Trait|Type/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[subset :: of Int;]),
    'Type::Subset@0-16 Name@7-9 Trait::Of@10-16 Type::Simple@13-16 Name@13-16',
    'an anonymous subset statement';
is placed(Q[my $y = (subset T of Int);]),
    'Type::Subset@9-24 Name@16-17 Trait::Of@18-24 Type::Simple@21-24 Name@21-24',
    'a named subset term';
is placed(Q[my $z = 1 ~~ (subset :: of Int where * > 0);]),
    'Type::Subset@14-42 Name@21-23 Trait::Of@24-30 Type::Simple@27-30 Name@27-30',
    'an anonymous subset term with a where clause';
