# `return` from nested blocks and from a supply block's `whenever`

A `return` inside a block that was itself created while another block was
running now leaves the enclosing routine. Every closure invocation, block or
routine, binds its own id as `__mutsu_callable_id` (`once`, `leave` and
flip-flop state key on it), so an inner block captured the *outer block's* id as
its return target. No routine frame answered to that id, and the signal escaped
as "Attempt to return outside of immediately-enclosing Routine":

```raku
sub f { my $b = { my $c = -> { return 5 }; $c() }; $b(); 6 }
say f();   # 5 (was: the X::ControlFlow::Return error)
```

A block invocation now also records the routine its own `return` targets,
tagged with the block's id (`runtime/return_target.rs`); the tag stops a routine
invoked from inside the block from inheriting it. Every site that stamps a
block's `return` with its target — closure dispatch, bare-block calls, lazy
`map`/`gather` bridges — resolves through that record.

The same bug made a `return` in a `whenever` of an on-demand `supply { }` fail
when the supply was tapped by `react` (#9630): the `whenever` is created while
the supply block runs. Tapped through `.list`, the `return` was lost instead:
the cold-source replay (`drive_whenever_body_over_values`) dispatched the body
with `call_sub_value`, whose routine-style boundary swallowed the `return`, and
treated any error as a quit. It now dispatches through `call_react_callback`,
like the react loop, and propagates a `return` to the tapping routine.

```raku
sub f { my $s = supply { whenever Supply.from-list(1,2) -> $x { return $x } }; say $s.list; 5 }
say f();   # 1 (was: () then 5)
```
