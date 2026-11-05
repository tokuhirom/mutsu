use Test;

# A `;` that ends no statement inside the rest of a `unit` declaration is a
# `Statement::Empty` with the range rakudo 2026.09 gives it (#15780).

plan 6;

use experimental :rakuast;

sub rng($n) { my $o = $n.origin; $o ?? "{$o.from}-{$o.to}" !! "none" }
sub items(Str $src) {
    my $list = $src.AST.statements[0].expression.body.body.statement-list;
    $list.statements.map({ .^name.subst('RakuAST::', '') ~ ":" ~ rng($_) }).join(" ")
}

is items("unit module EmA;\n;\nsub f \{ 1 }\n"),
    "Statement::Empty:17-17 Statement::Expression:19-30", 'module: an empty statement first';
is items("unit module EmB;\nsub f \{ 1 }\n;;\n"),
    "Statement::Expression:17-28 Statement::Empty:30-30", 'module: after the last statement';
is items("unit module EmC;\nsub f \{ 1 };\nsub g \{ 1 };;\nsub h \{ 1 }\n"),
    "Statement::Expression:17-28 Statement::Expression:30-41 Statement::Empty:42-42 Statement::Expression:44-55",
    'module: between statements';
is items("unit class EmD;\nmethod m \{ 1 };\n;\n"),
    "Statement::Expression:16-30 Statement::Empty:32-32", 'class: after the last statement';
is items("unit role EmE;\nmethod m \{ 1 };;\n").contains("Statement::Empty:"), True,
    'role: the empty statement has a range';
is items("unit grammar EmF;\ntoken t \{ a };;\n"),
    "Statement::Expression:18-31 Statement::Empty:32-32", 'grammar: after the last statement';
