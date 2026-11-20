use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a class, role or grammar
# written as an expression (`my $c = class { ... }`) places its body, its
# statements and its declarations as the statement form does; the `Name` of
# `class :: is Int { }` is the `::`, and its block starts at the first trait.

plan 5;

sub o($n) { $n.origin ?? $n.origin.from ~ '-' ~ $n.origin.to !! '-' }
sub term(Str $src) { $src.AST.statements[0].expression.initializer.expression }

my $c = term(Q[my $c = class { method a { 1 }; method b {} };]);
is o($c.body) ~ ' ' ~ $c.body.body.statement-list.statements.map({ o($_) ~ ':' ~ o(.expression) }).join(' '),
    '14-45 16-30:16-30 32-43:32-43', 'an anonymous class body and its methods';
$c = term(Q[my $c = class :: is Int { has $.x };]);
is o($c.name) ~ ' ' ~ o($c.body) ~ ' ' ~ o($c.traits[0]), '14-16 17-35 17-23', 'class :: with a trait';
$c = term(Q[my $c = class Foo { };]);
is o($c.name) ~ ' ' ~ o($c.body), '14-17 18-21', 'a named class as an expression';
$c = term(Q[my $r = role { method a {} };]);
is o($c.body), '13-28', 'an anonymous role body';
$c = term(Q[my $g = grammar { token TOP { a } };]);
is o($c.body) ~ ' ' ~ o($c.body.body.statement-list.statements[0].expression.name), '16-35 24-27',
    'an anonymous grammar body and its token';
