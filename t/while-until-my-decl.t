use v6;
use Test;

# `while`/`until` with a bare `my`/`our`/`state` declaration in the condition.
# The declaration must run once at loop entry, not be re-declared (and reset)
# on each iteration — otherwise a body that mutates the variable can never make
# the condition true. Regression for `until my $done { ... }`.

plan 7;

# until my $done — body sets $done to terminate
{
    my $i = 0;
    until my $done {
        $i++;
        $done = True if $i >= 3;
    }
    is $i, 3, 'until my $done loops until body sets the declared variable';
}

# while my $done — runs while the declared variable is false
{
    my $i = 0;
    while my $done {
        $i++;
    }
    is $i, 0, 'while my $done does not run while the declared variable is false';
}

# until my $done with a later body that flips it
{
    my @log;
    until my $stop {
        @log.push(@log.elems);
        $stop = True if @log.elems == 4;
    }
    is @log, [0, 1, 2, 3], 'until my keeps the declared variable across iterations';
}

# The with-initializer idiom must keep re-running the initializer each iteration.
{
    my @data = 1, 2, 3;
    my $idx = 0;
    my @got;
    while my $x = @data[$idx++] {
        @got.push($x);
    }
    is @got, [1, 2, 3], 'while my $x = expr re-runs the initializer each iteration';
}

# A genuine hash subscript on an already-declared variable still parses.
{
    my %h = a => 1, b => 2;
    is %h{'a'}, 1, 'hash subscript on a declared variable still works';
}

# The body of a `my`-declaration loop condition must still parse normally,
# including hash subscripts inside the body (the declaration's trailing block is
# the loop body, but subscripts elsewhere are unaffected).
{
    my %h = a => 0;
    until my $done {
        %h{'a'} = %h{'a'} + 1;
        $done = True if %h{'a'} >= 3;
    }
    is %h{'a'}, 3, 'hash subscripts inside a my-condition loop body still work';
}

# until my $done with no body mutation but an external break via last
{
    my $n = 0;
    until my $never {
        $n++;
        last if $n == 10;
    }
    is $n, 10, 'until my with last works';
}
