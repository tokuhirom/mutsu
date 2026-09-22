# An `is rw` routine's conditional tail returns the taken branch's container

An `is rw` routine returns the container its last *executed* expression names.
ADR-0059 Slice 2 compiled a routine's bare tail expression as that container
(`rw_tail`, via `compile_return_rw_arg`), but only when the tail was a plain
expression statement. When the tail was an `if`/`elsif`/`else`, a `given`, or a
`when`/`default` clause, the branch's own tail compiled as an ordinary value,
so assignment through the call failed:

```raku
my %returning = name => "old";
my $meth = method (|c) is rw {
    if %overriding<name> -> $o { $o }
    else { %returning<name> }
};
Mocker.HOW.add_method(Mocker, "name", $meth);
Mocker.new.name = "new";   # mutsu: X::Assignment::RO; raku: writes %returning<name>
```

That is the exact shape `Test::Mock`'s `mocked()` builds for every `is rw` method
it overrides, and it stopped `App::six-pm`'s own test suite
([#9060](https://github.com/tokuhirom/mutsu/issues/9060)).

The fix does not add another shape to the static `rw_method_attribute_target`
pattern list. Instead the compiler carries the routine-tail position down into
the conditional: a new `rw_tail_branch` flag is set when an `is rw` routine's
tail statement is an `if` or a `given`, and it is handed to each branch's
value list, each `when`/`default` clause, and an `elsif`'s nested `if`. The
statement that is last in that list compiles through `compile_return_rw_arg`,
exactly as a bare tail does. The flag is *taken* (not just read) at each level,
so it never reaches a condition, a non-tail statement, or an expression nested
inside the tail. The runtime path that was already there
(`assign_lvalue_container` on the method's result) then writes through the
returned container with no change.

Pinned by `t/oo/method/rw-method-conditional-tail-container.t`, which also
checks that non-`is rw` routines and non-tail conditionals still decontainerize.
