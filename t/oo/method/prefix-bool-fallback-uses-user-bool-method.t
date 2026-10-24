use v6;
use Test;

plan 8;

# With a user `prefix:<?>`/`prefix:<!>`/`prefix:<so>`/`prefix:<not>` in scope,
# an operand no candidate takes falls back to the core operator, which asks
# the operand's own `Bool` method.
class AST { }
class Other { method Bool { False } }
class Yes { method Bool { True } }
multi prefix:<?>(AST $a) { "ast-q" }
multi prefix:<!>(AST $a) { "ast-not" }
multi prefix:<so>(AST $a) { "ast-so" }
multi prefix:<not>(AST $a) { "ast-not2" }

my $o = Other.new;
my $y = Yes.new;
is ?$o, False, "? on an object whose Bool is False";
is !$o, True, "! on it";
is so($o), False, "so on it";
is not($o), True, "not on it";
is ?$y, True, "? on an object whose Bool is True";
is !$y, False, "! on it";
is ?AST.new, "ast-q", "a matching candidate still wins";
is !AST.new, "ast-not", "also for !";
