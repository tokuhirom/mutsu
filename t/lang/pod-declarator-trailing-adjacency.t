use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# A trailing `#=` documents the declaration before it only while it stays next
# to it line by line (rakudo 2026.11): it must start on the line the
# declaration's text ends on, the line after it, or the line after the start of
# the previous `#=` that documented the same declaration. A blank line, an
# ordinary comment or a multi-line `#=[...]` block cuts the chain, and the
# comment past it has no declarand: rakudo warns "Missing declarand for
# trailing declarator doc" and attaches it to nothing. Routines and grammar
# rules are exempt: blank lines do not cut them off (#18096).
#
# The worry is the program's own: rakudo reports a module's worries once, when
# it precompiles it, never on a later load, while mutsu replays a module's
# parse warnings on every load -- so a module only gets the attachment rule.
# (Test 15 therefore differs from a *cold* rakudo, which prints the worry on
# the precompiling load; roast's 6.c/MISC/bug-coverage-stress.t relies on a
# module load staying quiet.)

plan 15;

my $missing = 'Missing declarand for trailing declarator doc';

is_run ｢
class A {
    method m { }
}
#=[ trailing A ]

#=[ more for A,
    second block ]
my class B { }
print A.WHY, '|', B.WHY.defined;
｣, %(:out('trailing A|False'), :err{ .contains($missing) && .contains(':8') }),
    'a #= block after a blank line is not appended to the previous declarand';

is_run ｢
class A {
}
#=[ one ]
#=[ two ]
print A.WHY;
｣, %(:out('one two'), :err('')), 'adjacent #= lines all document the class';

is_run ｢
class A {
}

#= one
print A.WHY.defined;
｣, %(:out('False'), :err{ .contains: $missing }), 'a blank line after a class cuts it off';

is_run ｢
class A {
}
# plain
#= one
print A.WHY.defined;
｣, %(:out('False'), :err{ .contains: $missing }), 'so does an ordinary comment line';

is_run ｢
class A {
}
#=[ one
  more ]
#= two
print A.WHY;
｣, %(:out('one more'), :err{ .contains: $missing }),
    'the chain counts from where the previous #= starts, not where it ends';

is_run ｢
sub f {
}


#= one
#= two

#= three
print &f.WHY;
｣, %(:out('one two three'), :err('')), 'blank lines do not cut a routine off';

is_run ｢
class C {
  method m {}

  #= one
}
print C.^find_method('m').WHY;
｣, %(:out('one'), :err('')), '... nor a method';

is_run ｢
class C {
  has $.x;

  #= one
}
print C.^attributes[0].WHY.defined, C.WHY.defined;
｣, %(:out('FalseFalse'), :err{ .contains: $missing }),
    'an attribute is cut off, and the comment does not fall back to the class';

is_run ｢
class C {
  #= doc
  #= more

  #= third
}
print C.WHY;
｣, %(:out('doc more'), :err{ .contains: $missing }),
    'inside a package body the chain starts at the package line';

is_run ｢
class C {
  say 1;
  #= doc
}
print C.WHY.defined;
｣, %(:out("1\nFalse"), :err{ .contains: $missing }),
    'a #= after a non-declaration statement in a body documents nothing';

is_run ｢
unit class C;
#= doc

#= more
print C.WHY;
｣, %(:out('doc'), :err{ .contains: $missing }), 'a unit package';

is_run ｢
say 1;
#= foo
｣, %(:out("1\n"), :err{ .contains: $missing }), 'a #= with no declaration at all';

is_run ｢
{
    my \s = 'inner';
    #= doc
}
my ($a, $b) = 1, 2;
#= doc
print 'ok';
｣, %(:out('ok'), :err('')), 'sigilless and grouped declarations are declarands';

is_run ｢
grammar G {
  token t { a }

  #= one
}
print G.^find_method('t').WHY;
｣, %(:out('one'), :err('')), 'a grammar rule is exempt too';

{
    my $dir = make-temp-dir;
    $dir.add('DeclDocAdjacency.rakumod').spurt: ｢
unit module DeclDocAdjacency;
class A is export {
}
#= trailing A

#= stray
｣;
    is_run ｢use DeclDocAdjacency; print A.WHY｣, :compiler-args['-I', $dir.absolute],
        %(:out('trailing A'), :err('')), 'a module gets the rule, but not the worry';
}
