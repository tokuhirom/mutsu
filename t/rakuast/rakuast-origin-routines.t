use Test;

# Routine declarations carry the origins the parser read them from
# (ADR-10723 Stage 2): the routine, its name, signature and parameters, its
# blockoid and statement list. Ranges measured on rakudo 2026.09.

plan 21;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }

my $src = q:to/END/;
my sub foo( Int $x is copy = 3, :$y! ) { $x }
multi sub bar(\z) { }
class C { method !p(::?CLASS:D: @a, *%h) { } }
END
my $ast = $src.AST;
my @s = $ast.statements;

is range($ast), '0..114', 'the unit statement list';

my $foo = @s[0].expression;
is range($foo), '0..45', 'a sub, its scope declarator included';
is range($foo.name), '7..10', 'its name';
is range($foo.signature), '12..36', 'its signature, inside the parentheses';
my @p = $foo.signature.parameters;
is range(@p[0]), '12..30', 'a parameter with a type, a trait and a default';
is range(@p[0].target), '16..18', 'its target';
is range(@p[0].type), '12..15', 'its type';
is range(@p[1]), '32..36', 'a required named parameter';
is range(@p[1].target), '33..35', 'its target';
is range($foo.body), '39..45', 'the blockoid';
is range($foo.body.statement-list), '41..43', 'its statement list';

my $bar = @s[1].expression;
is range($bar), '46..67', 'a multi sub';
is range($bar.signature.parameters[0].target), '61..62', 'a sigilless target';

my $m = @s[2].expression.body.body.statement-list.statements[0].expression;
is range($m), '78..112', 'a private method';
is range($m.name), '86..87', 'its name, without the !';
is range($m.signature), '88..107', 'a signature with an invocant';
my @mp = $m.signature.parameters;
is @mp.map(&range).join(' '), '88..98 100..102 104..107', 'its parameters';
is range(@mp[0].type), '88..98', 'the invocant type, smiley included';
is range($m.body.statement-list), '110..110', 'an empty statement list';

# --- a hand-built or evaluated tree is unaffected --------------------------
is EVAL(q[sub f(Int $a, $b = 2) { $a + $b }; f(1)].AST), 3, 'the round trip still runs';
nok RakuAST::Sub.new(name => RakuAST::Name.from-identifier('g')).origin.defined,
    'a hand-built routine has no origin';
