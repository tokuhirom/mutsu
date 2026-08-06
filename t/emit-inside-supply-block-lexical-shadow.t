use Test;

# A lexical `&emit` must shadow the `supply { ... }` control-flow sugar even
# for a bare `emit(...)`/`emit ARGS` written *directly inside* the block's own
# body -- not just from a closure the body calls out to (that shape is
# covered by t/lexical-shadows-builtin-call.t). See
# todo/tickets/emit-inside-supply-block-ignores-a-lexical.md.
#
# `supply { ... }` lowers its body to `Supply.on-demand(-> $emitter { ... })`
# and the parser rewrites a *recognised* `emit` syntax (a bare statement, a
# `.emit` topic call) straight to `$emitter.emit(...)` at parse time -- but
# only when no lexical `&emit` shadows it. An `emit(...)` used as a
# sub-expression (assigned to a variable, inside a ternary, ...) is never
# rewritten at all and reaches the real `emit` builtin at runtime instead,
# so that builtin must also recognise a lexical override.

plan 6;

{
    my &emit = { "lexical:{$_[0]}" };
    my $s = supply {
        my $r = emit(42);
        emit($r);
    }
    my @tapped;
    $s.tap({ @tapped.push($_) });
    is-deeply @tapped, [], 'emit(...) assigned to a var calls the lexical, not the supply';
}

{
    my &emit = { "lexical:{$_[0]}" };
    my $s = supply {
        emit 42;
    }
    my @tapped;
    $s.tap({ @tapped.push($_) });
    is-deeply @tapped, [], 'bare statement-form `emit ARGS;` calls the lexical, not the supply';
}

{
    my &emit = { "lexical:{$_[0]}" };
    my $s = supply {
        my $r = True ?? emit(7) !! 'nope';
    }
    my @tapped;
    $s.tap({ @tapped.push($_) });
    is-deeply @tapped, [], 'emit(...) inside a ternary calls the lexical, not the supply';
}

{
    my @calls;
    my &emit = { @calls.push($_[0]) };
    my $s = supply {
        my $r = emit(42);
    }
    $s.tap({});
    is-deeply @calls, [42], 'the lexical &emit was actually invoked with the call argument';
}

# Regression: an unshadowed `emit` inside `supply { ... }` must still reach
# the block's own emitter, in all three of the shapes exercised above.
{
    my $s = supply {
        my $r = emit(1);
        emit 2;
        my $r2 = True ?? emit(3) !! 'nope';
    }
    my @tapped;
    $s.tap({ @tapped.push($_) });
    is-deeply @tapped, [1, 2, 3], 'an unshadowed emit still reaches the supply in every shape';
}

# Regression: `emit` reached from a *nested sub* (not textually inside the
# block) must still use the dynamically-enclosing supply -- the lexical-env
# check in the emit builtin fallback must not accidentally intercept this.
{
    sub relay($x) { emit $x }
    my $s = supply { relay(1); emit 2 }
    my @tapped;
    $s.tap({ @tapped.push($_) });
    is-deeply @tapped, [1, 2], 'emit from a nested sub still reaches the dynamically-enclosing supply';
}

# vim: expandtab shiftwidth=4
