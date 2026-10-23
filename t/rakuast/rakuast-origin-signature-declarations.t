use Test;

# The signature of a `my (...)` declaration, its parameters and its
# initializer carry the origins Rakudo gives them (ADR-10723 Stage 2). A
# signature mutsu cannot read back from the text is left without ranges.
# Ranges measured on rakudo 2026.09.

plan 19;

sub range($node) { with $node.origin { "{.from}..{.to}" } else { 'none' } }
sub parts($node) { my @c; $node.visit-children(-> $c { @c.push: $c }); @c }

my $src = q:to/END/;
my ($a, $b) = 1, 2;
my ( $c , @d );
my (Int $e, Str $f) = 1, "x";
our ($g, $h) := 1, 2;
sub k(Int $p, $q) { }
END
my @s = $src.AST.statements;

my @p = parts(@s[0].expression);
is range(@p[0]), '4..10', 'the signature';
is range(@p[1]), '12..18', 'the initializer';
my @params = parts(@p[0]);
is range(@params[0]), '4..6', 'a parameter';
is range(@params[1]), '8..10', 'another parameter';
my @target = parts(@params[0]);
is range(@target[0]), '4..6', 'its target';
is range(parts(@target[0])[0]), '4..6', 'and the declaration the target visits';

@p = parts(@s[1].expression);
is range(@p[0]), '25..32', 'a signature with padding stays trimmed';
is range(parts(@p[0])[1]), '30..32', 'a parameter after padding';

@p = parts(@s[2].expression);
@params = parts(@p[0]);
is range(@p[0]), '40..54', 'a typed signature';
is range(@params[0]), '40..46', 'a typed parameter';
my @typed = parts(@params[0]);
is range(@typed[0]), '40..43', 'its type';
is range(parts(@typed[0])[0]), '40..43', 'and the type name';
is range(@typed[1]), '44..46', 'its target';
is range(parts(parts(@typed[1])[0])[0]), '40..43', 'and the type the declaration holds';

@p = parts(@s[3].expression);
is range(@p[0]), '71..77', 'a signature of our variables';
is range(@p[1]), '79..86', 'a binding initializer';

# A routine's untyped parameter declares a variable at its target.
my $parameter = parts(parts(@s[4].expression)[1])[0];
is range(parts(parts($parameter)[1])[0]), '98..100', 'a routine parameter declaration';

# An anonymous variable is placed at its sigil.
my $anonymous = 'my ($, $z) = 1, 2;'.AST.statements[0].expression;
is range(parts($anonymous)[0]), '4..9', 'a signature with an anonymous variable';
is range(parts($anonymous)[1]), '11..17', 'but its initializer keeps its range';
