use Test;
use experimental :rakuast;

# `RakuAST::Node.DEPARSE` runs rakudo's own `RakuAST::Deparse` (vendored
# verbatim, ADR-14845); the expected strings are what `raku` prints for the
# `.AST` of each snippet.

my @cases =
  "say 1 + 2;" => "say 1 + 2\n",
  "my \@a = 1..3; for \@a -> \$x \{ say \$x }" => "my \@a = 1 .. 3;\nfor \@a -> \$x \{\n    say \$x\n}\n",
  "sub f(\$x, :\$y = 2) \{ return \$x * \$y }; f(21);" => "sub f (\$x, :\$y = 2) \{\n    return \$x * \$y\n}\nf(21)\n",
  "my (\$a, \$b); if \$a \{ 1 } elsif \$b \{ 2 } else \{ 3 }" => "my (\$a, \$b);\nif \$a \{\n    1\n}\nelsif \$b \{\n    2\n}\nelse \{\n    3\n}\n",
  "my \$s = \"a\" ~ 'b'; say \"x\{\$s}y\";" => "my \$s = \"a\" ~ \"b\";\nsay \"x\{ \$s }y\"\n",
  "class A \{ has \$.x = 1; method m \{ self.x } }" => "class A \{\n    has \$.x = 1;\n    method m \{\n        self.x\n    }\n}\n",
  "my \%h = a => 1, b => 2; say \%h<a>;" => "my \%h = a => 1, b => 2;\nsay \%h<a>\n",
  "my \$i = 0; while \$i < 3 \{ \$i++ }" => "my \$i = 0;\nwhile \$i < 3 \{\n    \$i++\n}\n",
  "given 1 \{ when 1 \{ say \"one\" }; default \{ say \"other\" } }" => "given 1 \{\n    when 1 \{\n        say \"one\"\n    }\n    default \{\n        say \"other\"\n    }\n}\n",
  "my \@l = (1, 2, 3).map(* * 2).grep(* > 2); say \@l.elems;" => "my \@l = (1, 2, 3).map(* * 2).grep(* > 2);\nsay \@l.elems\n",
  "sub g(Int \$n where * > 0 --> Str) \{ \"pos\" }; say g(3);" => "sub g (Int \$n where * > 0 --> Str) \{\n    \"pos\"\n}\nsay g(3)\n",
  "loop (my \$i = 0; \$i < 3; \$i++) \{ next if \$i == 1; say \$i }" => "loop (my \$i = 0; \$i < 3; \$i++) \{\n    next if \$i == 1;\n    say \$i\n}\n",
  "my \$f = -> \$a, \$b \{ \$a + \$b }; say \$f(1, 2);" => "my \$f = -> \$a, \$b \{\n    \$a + \$b\n};\nsay \$f(1, 2)\n",
  "multi sub m(Int \$a) \{ 1 }; multi sub m(Str \$a) \{ 2 }; say m(1);" => "multi sub m (Int \$a) \{\n    1\n}\nmulti sub m (Str \$a) \{\n    2\n}\nsay m(1)\n";

plan @cases + 5;

for @cases -> $case {
    is $case.key.AST.DEPARSE, $case.value, "deparses {$case.key.raku}";
}

# a class takes the place of the default deparser, a role is mixed into it
my class Shouting is RakuAST::Deparse {
    method list-infix-comma(--> Str:D) { ' ;; ' }
}
isa-ok RakuAST::Deparse.new, RakuAST::Deparse, 'RakuAST::Deparse is a class';
is Q|say 1, 2|.AST.DEPARSE(Shouting), "say 1 ;; 2\n", 'a class replaces the deparser';

role Dotted { method statement-delimiter(--> Str:D) { ".\n" } }
like Q|say 1; say 2|.AST.DEPARSE(Dotted), /^ 'say 1' .+ 'say 2'/, 'a role is mixed in';

# the L10N localizations read the same table the other way
is Q|my $x = 1|.AST.DEPARSE.chars > 0, True, 'DEPARSE answers a Str';
is RakuAST::IntLiteral.new(3).DEPARSE, '3', 'a hand-built node deparses';
