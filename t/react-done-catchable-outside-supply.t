use Test;
plan 5;

# `emit`/`done` outside any supply/react must be an ordinary *catchable*
# X::ControlFlow -- `try { emit 1 }` has to leave `$!` set, not let the
# signal escape uncaught past `try` (which is what mutsu's native `try`
# previously did: `is_illegal_control()` never matched `Control::Emit` /
# `Control::ReactDone`, so the two `while`/`Err` arms in
# `vm_try_catch_ops.rs` that forward every emit/done signal past `try`
# forwarded even the "nothing will ever consume this" case, and it
# escaped the whole program as an uncaught Rust-level Runtime error).
# See todo/deep/vendor-real-test-module.md ("t/emit-done-controlflow.t" /
# "t/take-without-gather.t" real-Test regressions).

try { emit 1 };
is $!.^name, 'X::ControlFlow', 'try catches a bare emit outside supply/react';

try { done };
is $!.^name, 'X::ControlFlow', 'try catches a bare done outside supply/react';

# Regression guard: a *legitimate* `done` -- one with a dynamically
# enclosing supply/react to terminate -- must still propagate through a
# `try` wrapping it, rather than being caught right there. This is the
# counterpart bug the naive "always catch ReactDone" fix would have
# introduced: `RuntimeError::done_signal()` (no exception) has to be used
# instead of `react_done_signal()` (exception set, matches
# `is_illegal_control()`) whenever `supply_emit_buffer`/`react_active`
# show a drive loop is dynamically active.
{
    my @got;
    my $s = supply {
        whenever Supply.from-list(1, 2, 3) -> $v {
            try {
                emit $v;
                done if $v == 2;
            }
        }
    };
    $s.tap(-> $x { @got.push($x) }, done => { @got.push('done') });
    is @got.join(','), '1,2,done',
        'a try-wrapped done with an active supply still terminates it, not caught by the try';
}

# Same shape one level deeper: `done` from a `try` inside a `whenever`
# nested inside a `react` block.
{
    my @got;
    react {
        whenever Supply.from-list(1, 2, 3) -> $v {
            try {
                @got.push($v);
                done if $v == 2;
            }
        }
    }
    is @got.join(','), '1,2',
        'a try-wrapped done inside a react whenever still ends the react loop';
}

# A third dispatch path: a `whenever` on a *live* Supplier-backed supply
# (not a static `from-list`) invokes its body through
# `Interpreter::call_supply_tap`, a third producer distinct from the two
# above -- each of the three needed its own `ReactDoneHandlerGuard`.
{
    my @got;
    my $sup = Supplier.new;
    my $s = supply {
        whenever $sup -> $v {
            try {
                emit $v * 2;
                done if $v == 2;
            }
        }
    };
    $s.tap(-> $x { @got.push($x) }, done => { @got.push('done') });
    $sup.emit(1);
    $sup.emit(2);
    $sup.emit(3);
    is @got.join(','), '2,4,done',
        'a try-wrapped done on a live Supplier-backed whenever still terminates it';
}
