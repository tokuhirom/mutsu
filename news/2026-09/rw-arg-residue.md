# The subscript-argument container producer's last three rows

`todo/tickets/subscript-argument-container-producer.md`'s headline was closed by
ADR-0067's `OpCode::IndexArgRef` (`news/2026-09/slice-first-and-block-topic-element-containers.md`);
this closes the three rows it left open, each measured against raku.

## A `Method` value invoked as a code value

```raku
class C { has $.v is rw }
class M { method m($y is rw) { $y = 9 } }
my $c = C.new(v => 1); my $obj = M.new;
$obj.^lookup('m')($obj, $c.v);   # raku: $c.v is 9   mutsu: refused
```

Such a method takes its invocant as positional argument 0, so the `is rw`
parameter is positional **1 counting the invocant** — but the code-value gate
`code_value_binds_container_at` skipped invocant parameters and looked one
parameter too far along. Two things were needed. `.^lookup` hands back a
`Method` *Instance* (ADR-0019 Phase F box F1) whose `__mutsu_method_callable`
attribute is the routine `CALL-ME` runs, and the gate only understood a bare
`Sub`, so it declined before reaching the counting question at all. It now
unwraps that attribute, and treats reaching it as the signal that the invocant
IS one of the counted arguments. Every other callable — a `sub`, a block, a
method reached through ordinary `.`-dispatch — receives its invocant out of
band, so the skip stays for them.

## `is rw` on an optional NAMED parameter

`sub g(:$y is rw) { }` is a compile-time error in raku
(`Cannot use 'is rw' on optional parameter '$y'`); mutsu accepted the
declaration and refused at the call site instead. The validation existed
(`src/parser/stmt/sub/param_validate.rs`) but tested only `optional_marker ||
default.is_some()` — and a *named* parameter is optional without either, unless
it carries `!`. `:$y! is rw` is still accepted, and the `method` spelling's error came for
free. One local test declared `sub b(:x($a) is rw, ...)` for introspection —
a declaration rakudo also refuses — and was moved to the `!`-required
spelling, under which every one of its assertions is unchanged (measured).

## An out-of-range subscript argument

```raku
my @a = 1, 2; sub g($y is rw) { $y = 9 }; my $r = &g; $r(@a[5]);
# raku: [1 2 (Any) (Any) (Any) 9]     mutsu: died / silently dropped
```

The ticket framed this as a decision rather than a fix: `IndexArgRef` shared
`take_subscript_element_cell` with the *receiver* producer, whose contract is
"an existing element of a real Array/Hash", so either the `is rw` and topic
binders learn to accept a deferred vivification token, or the argument producer
gets its own eager-growth gate.

**The argument producer gets its own gate.** An argument position is a
*definite bind*, while a receiver (`@a[5].mut`) is not and legitimately has no
element — and mutsu's own NAMED-callee path already grew the array here, through
`CallFunc`'s copy-in/copy-out temp protocol (`g(@a[5])` was already right). So
the divergence is deliberate and now stated: the shared producer takes a `grow`
flag, set only by `IndexArgRef`.

On the positional side that is just `array_slot_ref`'s non-terminal (eager)
mode. On the associative side `terminal` cannot express it — a missing key
defers either way — so the entry is created and the slot re-taken, leaving an
existing entry's `terminal: true` promotion of a nested Array/Hash element
untouched. Both halves now answer as raku does for a code-value callee and for
a bare-block topic, and a plain out-of-range *read* still leaves the array
alone.

Pinned by `t/rw-arg-residue.t`, whose 14 assertions pass unchanged under rakudo.
