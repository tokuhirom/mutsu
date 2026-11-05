use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

# #16787: `EVAL($ast)` of a tree whose declarations carry declarator docs makes
# `.WHY` answer them, as the same source text does: a sub, a package, a method
# (with a trailing comment), an attribute, a grammar rule, `multi` candidates
# and role variants. The docs were lost when the tree was lowered.

plan 12;

my $src = Q[#| a sub
sub k {}
#| a multi one
multi sub m(Int $x) {}
multi sub m(Str $x) {}
#| a multi two
multi sub m(Num $x) {}
#| a class
class A {
  #| a method
  method m1 {} #= trailing m1
  #| an attr
  has $.at;
  #| a rule
  grammar G { token tk { a } }
}
#| role one
role R {}
#| role two
role R[::T] {}
sub undoc {}
my %r;
%r<sub>     = &k.WHY.Str;
%r<class>   = A.WHY.Str;
%r<method>  = A.^find_method('m1').WHY.Str;
%r<trail>   = A.^find_method('m1').WHY.trailing;
%r<undoc>   = &undoc.WHY.defined;
%r<multi>   = (&m.candidates.map({ .WHY.defined ?? .WHY.Str !! '-' })).join(',');
%r<role>    = R.WHY.Str;
%r;
];

my %r = EVAL($src.AST);
# the same source under other package names, so both units can declare them
my %s = EVAL($src.subst(/<|w>A<|w>/, 'B', :g).subst(/<|w>R<|w>/, 'S', :g).subst(/<|w>G<|w>/, 'H', :g));
for <sub class method trail undoc multi role> -> $k {
    is %r{$k}, %s{$k}, "EVAL of the tree answers what the source does: $k";
}
is %r<sub>, 'a sub', 'a sub keeps its docs';
is %r<multi>, 'a multi one,-,a multi two', 'multi candidates are told apart';
is %r<role>, 'role one', 'the first role variant';
is %r<trail>, 'trailing m1', 'a trailing comment';
is %r<undoc>, False, 'an undocumented sub has none';
