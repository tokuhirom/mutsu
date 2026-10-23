use Test;

# Routine and parameter traits carry the origins the parser read them from
# (ADR-10723 Stage 2): the trait, and the name or type it names. Ranges
# measured on rakudo 2026.09.

plan 15;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub first-child($node) { my @c; $node.visit-children(-> $c { @c.push: $c }); @c[0] }

my $src = q:to/END/;
sub f(Int $x is copy is raw = 3) returns Int is export { $x }
class C { method m($a is raw) of Str is DEPRECATED { } }
END
my @s = $src.AST.statements;

my $f = @s[0].expression;
my @ft = $f.traits;
is range(@ft[0]), '33..44', 'a returns trait';
is range(first-child(@ft[0])), '41..44', 'its type';
is range(@ft[1]), '45..54', 'an is trait on a sub';
is range(first-child(@ft[1])), '48..54', 'its name';

my @pt = $f.signature.parameters[0].traits;
is range(@pt[0]), '13..20', 'a parameter trait';
is range(first-child(@pt[0])), '16..20', 'its name';
is range(@pt[1]), '21..27', 'a second parameter trait';
is range(first-child(@pt[1])), '24..27', 'its name';

my $m = @s[1].expression.body.body.statement-list.statements[0].expression;
my @mt = $m.traits;
is range(@mt[0]), '92..98', 'an of trait on a method';
is range(first-child(@mt[0])), '95..98', 'its type';
is range(@mt[1]), '99..112', 'an is trait on a method';
is range(first-child(@mt[1])), '102..112', 'its name';
is range($m.signature.parameters[0].traits[0]), '84..90', 'a method parameter trait';

# --- a hand-built or evaluated tree is unaffected --------------------------
is EVAL(q[sub g($a is copy) returns Int { $a += 1; $a }; g(41)].AST), 42,
    'the round trip still runs';
nok RakuAST::Trait::Is.new(name => RakuAST::Name.from-identifier('rw')).origin.defined,
    'a hand-built trait has no origin';
