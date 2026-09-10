use Test;

# A compound (or plain) scalar assignment used as a method-call argument must
# evaluate to the assigned VALUE, not a Pair. `compile_method_arg_with_escape`
# used to special-case an `AssignExpr` whose `name` field lacked a `$`/`@`/`%`/
# `&` sigil prefix as a "named argument" (`foo(arg = 1)` -> `:arg(1)`
# sugar) -- but `AssignExpr.name` never carries the `$` sigil for a genuine
# scalar target (`$x = ...`/`$x += ...`, only `@`/`%` targets get one
# prepended), so this misfired on every scalar assignment used as a method
# argument, turning `@r.push($x += 5)` into pushing the Pair `x => 5` instead
# of `5`. See todo/tickets/compound-assign-as-call-argument-yields-pair.md.

plan 8;

{
    my @r;
    my $x;
    @r.push($x += 5);
    is-deeply @r, [5], 'compound assignment as a method-call argument pushes the value';
}

{
    my @r;
    my $x = 0;
    @r.push(($x += 5));
    is-deeply @r, [5], 'parenthesized compound assignment as a method-call argument';
}

{
    my @r;
    my $x;
    @r.push($x = 7);
    is-deeply @r, [7], 'plain assignment as a method-call argument pushes the value';
}

{
    my $y;
    is ~($y ~= "z"), 'z', 'coercion-wrapped compound assignment still works (control case)';
}

{
    my @r;
    my $x;
    @r.push($x += 5);
    is $x, 5, 'the target variable itself is still updated by the compound assignment';
}

{
    # The anonymous per-routine-call state variable spelling ($) hits the same
    # compile path.
    sub f() {
        my @r;
        for 1..3 {
            @r.push($ += 5);
        }
        @r;
    }
    is-deeply f(), [5, 10, 15], 'anonymous state ($) compound-assigned as a method argument';
}

{
    # Regression: a genuine named-arg Pair (fat-arrow / colonpair) as a
    # method-call argument must still build a Pair, not fall through to a
    # plain positional.
    my %seen;
    my $obj = class {
        method tag(:$label) { $label }
    }.new;
    is $obj.tag(label => 'x'), 'x', 'fat-arrow named argument still works on a method call';
    is $obj.tag(:label('y')), 'y', 'colonpair named argument still works on a method call';
}

# vim: expandtab shiftwidth=4
