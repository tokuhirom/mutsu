use lib 't/lib';
use experimental :rakuast;
use Test;
use ImportedOperatorResolution;

# An operator a `use` imported resolves, in the unit's RakuAST, to that import
# (`Declaration::Import`), not to the setting's routine: its
# `compile-time-value` is the imported family, with the module's candidates.
# Red's CHECK compiler reads `.resolution` of every `Infix`/`Postfix` this way
# to decide whether a callback may be compiled (Red t/091-rakuast-compiler).

plan 6;

my @seen;
CHECK {
    my @statements = $*CU.statement-list.statements;
    my $probe = @statements.first({ .^name eq 'RakuAST::Statement::Expression'
        && .expression ~~ RakuAST::ApplyInfix && .expression.infix.operator eq '==' }, :k);
    for @statements[$probe, $probe + 1] -> $statement {
        my $e = $statement.expression;
        my $op = $e ~~ RakuAST::ApplyInfix ?? $e.infix !! $e.postfix;
        my $code = $op.resolution.compile-time-value;
        @seen.push: [$op.is-resolved, $op.resolution.^name,
            so $code.candidates.first(*.file.contains('ImportedOperatorResolution'))];
    }
}

my $x = 1;
$x == 2;
$x++;
my $tag = OpTag.new;

is @seen[0][1], 'RakuAST::Declaration::Import', 'an imported infix resolves to the import';
ok @seen[0][0], 'the infix is resolved';
ok @seen[0][2], "its compile-time value holds the module's candidate";
is @seen[1][1], 'RakuAST::Declaration::Import', 'an imported postfix resolves to the import';
ok @seen[1][2], "its compile-time value holds the module's candidate";
is $tag == 1, 'tag', 'the imported candidate still runs';
