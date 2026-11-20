use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a `{ ... }` block
# interpolated into a string is placed even when its code holds quoted
# strings, a brace inside one included.

plan 4;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) if $class ~~ /Block|StatementList|Statement|StrLiteral/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[say "x{ "a" ~ 'b' }y"]),
    'StatementList@0-21 Statement::Expression@0-21 StrLiteral@5-6 Block@6-19 Blockoid@6-19 StatementList@8-17 Statement::Expression@8-17 StrLiteral@9-10 StrLiteral@15-16 StrLiteral@19-20',
    'strings inside an interpolated block';
is placed(Q[my $v; say "x{ $v eq "}" }y"]),
    'StatementList@0-28 Statement::Expression@0-5 Statement::Expression@7-28 StrLiteral@12-13 Block@13-26 Blockoid@13-26 StatementList@15-24 Statement::Expression@15-24 StrLiteral@22-23 StrLiteral@26-27',
    'a quoted brace inside an interpolated block';
is placed(Q[say "{ 'don\'t' }"]),
    'StatementList@0-18 Statement::Expression@0-18 Block@5-17 Blockoid@5-17 StatementList@7-15 Statement::Expression@7-15 StrLiteral@8-14',
    'an escaped quote inside an interpolated block';
is placed(Q[my $v; $v."{'u' ~ 'c'}"()]),
    'StatementList@0-25 Statement::Expression@0-5 Statement::Expression@7-25 Block@11-22 Blockoid@11-22 StatementList@12-21 Statement::Expression@12-21 StrLiteral@13-14 StrLiteral@19-20',
    'an interpolated quoted method name';
