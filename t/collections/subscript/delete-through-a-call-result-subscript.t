use Test;

# `<expr>{key}:delete` where `<expr>` is an `is rw` CALL must delete from the
# container the call hands back. mutsu pushed that result on the stack and ran
# the generic `DeleteIndexExpr` over the popped value, so the deletion landed in
# a temporary and vanished -- while binding the identical expression to a
# variable and deleting through THAT worked. Crane does the direct form:
# `Crane::At.at($root, @path){$step}:delete`.

plan 9;

sub at-rw($c) is rw { return-rw $c<why> }

{
    my %h = :why({:do(1), :x(2)});
    at-rw(%h){'do'}:delete;
    is-deeply %h, {:why({:x(2)})}, 'delete through a return-rw sub result';
}
{
    my %h = :why({:do(1), :x(2)});
    at-rw(%h)<do>:delete;
    is-deeply %h, {:why({:x(2)})}, 'the angle-bracket spelling too';
}
{
    my %h = :why({:do(1), :x(2)});
    class At { method at($c) is rw { return-rw $c<why> } }
    At.at(%h){'do'}:delete;
    is-deeply %h, {:why({:x(2)})}, 'and through an rw method result';
}
{
    my %h = :why([10, 11, 12]);
    sub arr-rw($c) is rw { return-rw $c<why> }
    arr-rw(%h)[1]:delete;
    is %h<why>[1]:exists, False, 'a positional delete through a call result';
}

# The shapes that already worked must keep working.
{
    my %h = :why({:do(1), :x(2)});
    my $t := at-rw(%h);
    $t<do>:delete;
    is-deeply %h, {:why({:x(2)})}, 'delete through a bound temp (unchanged)';
}
{
    my %h = :why({:do(1), :x(2)});
    %h<why><do>:delete;
    is-deeply %h, {:why({:x(2)})}, 'a nested subscript delete (unchanged)';
}
{
    my %h = :a(1), :b(2);
    %h<a>:delete;
    is-deeply %h, {:b(2)}, 'a plain one-level delete (unchanged)';
}

# The delete still yields the removed value, and `:exists:delete` still reports
# existence.
{
    my %h = :why({:do(1), :x(2)});
    is (at-rw(%h){'do'}:delete), 1, 'the removed value is returned';
}
{
    my %h = :a(1);
    is (%h<a>:exists:delete), True, ':exists:delete still reports existence';
}
