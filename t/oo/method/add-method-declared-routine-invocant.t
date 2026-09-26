use Test;

# #9549: code handed to `^add_method` that is not a method takes the invocant
# as its first positional parameter. That failed for a DECLARED routine
# (`&named-sub`, `my sub`), which died "Too few positionals passed", and for
# a pointy block whose first parameter is literally `$self`, which read Nil.

plan 11;

class E { has $.v = 3 }
my $captured = 'c';
sub named-sub($o, $x) { "m7 {$o.v} $x" }
my sub lex-sub($o, $x, :$n = 1) { "ls {$o.v} $x $n $captured" }

E.^add_method('m7', &named-sub);
E.^add_method('ls', &lex-sub);
E.^add_method('m5', -> $self, $a { "m5 {$self.v} $a" });
E.^add_method('one', -> $self { $self.v + 1 });
E.^add_method('only', -> $r { $r.^name });
E.^add_method('opt', -> $o, $x = 5 { "opt {$o.v} $x" });
E.^add_method('cmp2', &[cmp]);

my $e = E.new;
is $e.m7(9), 'm7 3 9', 'a declared sub gets the invocant as its first parameter';
is $e.ls(4), 'ls 3 4 1 c', 'a my sub keeps its captures and named default';
is $e.ls(4, :n(9)), 'ls 3 4 9 c', '... and binds a named argument';
is $e.m5(2), 'm5 3 2', 'a pointy block first parameter named $self is the invocant';
is $e.one, 4, 'a one-parameter pointy block named $self';
is $e.only, 'E', 'a one-parameter pointy block with any name';
is $e.opt, 'opt 3 5', 'an optional parameter after the invocant keeps its default';
is $e.opt(6), 'opt 3 6', '... and binds a passed argument';
is-deeply $e.cmp2($e), Same, 'a builtin routine gets the invocant as its left operand';

is &named-sub.arity, 2, 'the declared sub itself still takes two arguments';
is named-sub($e, 1), 'm7 3 1', 'the declared sub still works called directly';
