use v6.e.PREVIEW;
use Test;

# `snitch` (Type/Any.rakudoc): a 6.e debugging probe that logs its invocant /
# argument and hands it straight back, so it can be spliced into the middle of
# a chain without changing the result. The logging goes to `$*ERR` via `note`
# unless a Callable replaces it.

plan 13;

# --- the method form returns the invocant unchanged --------------------------

{
    my $err = '';
    my $out;
    {
        my $*ERR = class { method print(*@a) { $err ~= @a.join }; method flush {} }.new;
        $out = (1..5).snitch;
    }
    is $err, "1..5\n", 'the method form notes the invocant gist to $*ERR';
    is $out, 1..5, 'and returns the invocant unchanged';
}

{
    my $err = '';
    my @result;
    {
        my $*ERR = class { method print(*@a) { $err ~= @a.join }; method flush {} }.new;
        @result = (1..3).Seq.snitch.map(*+2);
    }
    is $err, "(1 2 3)\n", 'a Seq is snitched as its own gist';
    is-deeply @result, [3, 4, 5], 'and the chain downstream of it is unaffected';
}

# --- a Callable replaces the default logger ---------------------------------

{
    my @snitched;
    my @result = (1..3).Seq.snitch({ @snitched.push($_) }).map(*+2);
    # Compared by gist: what `push`ing a Seq leaves in the array differs
    # between implementations, but what was snitched does not.
    is @snitched.gist, '[(1 2 3)]', 'a custom snitcher receives the invocant';
    is-deeply @result, [3, 4, 5], 'and the value still flows through';
}

{
    my $seen;
    my $back = 42.snitch(-> $v { $seen = $v });
    is $seen, 42, 'the snitcher is called with the invocant';
    is $back, 42, 'and snitch still returns it';
}

# --- the subroutine form puts the snitchee last -----------------------------

{
    my $err = '';
    my $out;
    {
        my $*ERR = class { method print(*@a) { $err ~= @a.join }; method flush {} }.new;
        $out = snitch(7);
    }
    is $err, "7\n", 'the sub form notes its argument';
    is $out, 7, 'and returns it';
}

{
    my $seen;
    my $out = snitch(-> $v { $seen = $v }, 'abc');
    is $seen, 'abc', 'snitch(&snitcher, \snitchee) uses the leading Callable';
    is $out, 'abc', 'and returns the trailing snitchee';
}

# --- 6.e only ---------------------------------------------------------------

{
    my $p = run $*EXECUTABLE, '-e', '(1..5).snitch', :err, :out;
    isnt $p.exitcode, 0, 'without `use v6.e.PREVIEW` there is no .snitch';
    $p.err.slurp(:close);
    $p.out.slurp(:close);
}

done-testing;
