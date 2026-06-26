use Test;

# §B (#3658 step 3): the writeback-safety gate in
# run_resolved_method_compiled_or_treewalk now permits a method that writes a
# captured-outer *lexical* to run as compiled bytecode (the merge of #3664
# preserves the write). Only dynamic ($*x) / special ($?x/$^x) twigil writes
# still force the tree-walk path. These cases exercise the resolved-candidate
# dispatch sites (multi-method, samewith, hyper) with a captured-outer write.

plan 7;

# multi-method candidate that mutates a captured-outer lexical.
my $log = '';
class M {
    multi method tag(Int $x) { $log ~= "i$x;"; self.tag($x.Str) }
    multi method tag(Str $x) { $log ~= "s$x;" }
}
M.new.tag(7);
is $log, 'i7;s7;', 'captured-outer write survives multi-method + self-redispatch (compiled)';

# A loop amplifies the same path (the defer-next shape) — the counter must
# accumulate across every iteration.
my $calls = 0;
class C {
    method bump { $calls++ }
}
my $c = C.new;
$c.bump for ^100;
is $calls, 100, 'captured-outer counter accumulates across 100 compiled method calls';

# A captured-outer lexical AND an rw public attribute mutated together.
my $total = 0;
class B {
    has $.n is rw;
    method step { $.n++; $total += $.n }
}
my $b = B.new(n => 10);
$b.step;
is $b.n, 11, 'rw attribute increment persists alongside a captured-outer write';
is $total, 11, 'captured-outer lexical sees the post-increment attribute value';

# Dynamic-var writing method still works (it stays on the tree-walk path; the
# gate keeps it there). Reduce-time grammar-action dynvar coherence is covered
# by t/grammar-reduce-time-dynvar.t; here just confirm a plain $*x write round-trips.
my $*DYN = 'orig';
class D { method setit { $*DYN = 'changed' } }
D.new.setit;
is $*DYN, 'changed', 'dynamic-var write from a method still propagates (tree-walk path)';

# Nested captured-outer writes (a method calling another method, both writing).
my @order;
class N {
    method outer { @order.push('o'); self.inner }
    method inner { @order.push('i') }
}
N.new.outer;
is @order.join(','), 'o,i', 'nested method calls each record their captured-outer write';

# String-magic captured-outer accumulation.
my $s = 'a';
class S { method advance { $s++ } }
my $sobj = S.new;
$sobj.advance; $sobj.advance;
is $s, 'c', 'captured-outer string increment accumulates across compiled calls';
