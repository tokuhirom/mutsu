use Test;
use OO::Monitors;

# A computed-name monitor method call ($m."$n"(...), CallMethodDynamicMut)
# inside a loop body still lost the caller's topic even after
# t/monitor-method-does-not-leak-topic-or-self.t fixed the general case: that
# fix excluded `$_`/`$/`/`$!`/`self` from the wrap-chain's persisted-env
# writeback, but exec_call_method_dynamic_mut_op only saved and restored
# `self` around the dispatch, not `$_` — so the dynamic-dispatch path itself
# (reached only via a *computed* name, and only manifesting inside a loop
# body) still clobbered the caller's topic.

plan 3;

monitor M { method d($p) { 1 } }

{
    my $m = M.new;
    my @got;
    for <d> -> $n {
        $_ = 'C';
        $m."$n"('x');
        @got.push($_);
    }
    is @got.join(','), 'C', 'computed-name monitor call in a loop keeps the caller topic';
}

{
    my $m = M.new;
    my @got;
    for <d> -> $n {
        $_ = 'E';
        $m.d('x');
        @got.push($_);
    }
    is @got.join(','), 'E', 'static-name monitor call in a loop keeps the caller topic (no regression)';
}

{
    my $m = M.new;
    $_ = 'D';
    $m."d"('x');
    is $_, 'D', 'computed-name monitor call at top level keeps the caller topic (no regression)';
}
