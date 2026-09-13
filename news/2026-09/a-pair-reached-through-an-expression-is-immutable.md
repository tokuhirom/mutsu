# A `Pair` reached through an expression is immutable, and an `is rw` routine that returns a plain value refuses

Five general divergences from rakudo, all found re-measuring [#7539](https://github.com/tokuhirom/mutsu/issues/7539)'s
`Config::TOML` + `Crane` battery pair. They share one theme: mutsu applied the
"this is not a location, refuse the write" rule only where the target was spelled
as a **variable**, and invented a container everywhere else. `Crane` is built
entirely on the everywhere-else spellings, so each divergence turned a refusal
Crane's `CATCH` depends on into a silent success.

`Crane` v0.1.2 goes from **4/15 files to 7/15** — `set`, `replace` and `remove`
now pass in full — with `Config::TOML` v0.1.3 unchanged at 14/19.

## 1. A type-object `is rw` method that returns a plain value fell back to the setter convention

`Crane::In.in(container, @path) = $value` is a class-method lvalue: mutsu runs
the method, and when it hands back a container writes through it. When it handed
back a *plain value* — which is exactly what a descent that has landed on an
immutable leaf does — mutsu reported the shape as inapplicable and let the legacy
`$obj.name($value)` setter convention take over. That convention **re-calls the
method with the assigned value as its only argument**, and with a `*@steps`
slurpy `in(9)` is a perfectly bindable call: the zero-step candidate handed `9`
straight back and the assignment reported success while writing nowhere.

The sub spelling of the same descent always refused. `try_rw_method_container_lvalue`
now shares the sub form's write half (`assign_through_rw_result`), so both answer
rakudo's `Cannot modify an immutable Int (1)`. A method that far in is rw-capable
and computes its location, so a plain value coming back **is** the refusal.

## 2. `return-rw <list>[i]` promoted an immutable `List`'s element to a private cell

`sub g(\c) is rw { return-rw c[0] }; g((1, 2)) = 9` wrote into a cell nothing
else could see; rakudo refuses with `Cannot modify an immutable Int (1)`, and an
out-of-range index with `Cannot modify an immutable Nil value` rather than growing
the `List`. The suppression already existed for a `:=` *declaration* bind
(`my $x := (5, 6)[0]`), gated by `IndexAutovivifyLazyTerminal`'s `decl_bind` flag.
A `return-rw` operand settles the caller's write from the element in exactly the
same way, so the flag now covers both and is renamed `raw_list_elem` to say what
it decides. A loop-parameter bind still keeps the promotion, which is what a
chunked `for @flat -> \a, \b` and `.kv` on a mutable QuantHash depend on.

An element that already IS a container is handed back untouched either way, so a
`List` built by `take-rw` is still written through.

## 3. A store into a `Pair` reached through an expression invented a container

`my $p = (c => True); id($p){'c'} = 9` (and the accessor spelling,
`Crane::At.at($root, @path){$step} = $value`) silently succeeded. A `Pair` DOES
`Associative`, so an associative subscript descends into it and rakudo refuses at
the value it reaches — the rule the chained-store walk on a variable already
followed. That arm is now a shared helper (`pair_subscript_store_refusal`) wired
into the computed-target store and the method-lvalue store as well.

Two supporting bugs in the accessor path came out with it. Its
`(root, @steps)` walk stepped through `Hash` and `Array` but answered `Nil` at a
`Pair`, so a colonpair chain — Crane's own fixture shape — was never reached; and
it unwrapped a `Scalar`/`ContainerRef` *inside* the per-step match, consuming a
step without descending, so an itemized root (`my $root = %h.deepmap(...)`, how
every non-in-place Crane operation builds its copy) came up one level short.

## 4. `<k>:delete` on a `Pair` removed nothing and reported success

Rakudo refuses every removal from a `Pair` with `X::AdHoc`
"Can not remove values from a Pair", the same shape it uses for a `Map`.
`Crane.remove` parses that payload back out (`Can not remove values from a (\w+)`)
to raise `X::Crane::Remove::RO`. The existing `refuse_map_removal` chokepoint —
which every delete path already calls — now covers `Pair` too.

## 5. `deepmap` boxed an already-promoted leaf a second time

`deepmap` passes each leaf to the block through a transient `ContainerRef` cell so
a mutating callable writes through. When the source element was *already* a cell
(promoted by an earlier `:=` bind or `is rw` descent), the block's `$_` became a
container around a container, and every method dispatched on the raw `$_` missed
it:

```raku
my %h = :a(:b(1)); my $x := %h<a>; %h.deepmap({ .clone })
# rakudo: {:a(:b(1))}
# mutsu:  No such method 'clone' for invocant of type 'Pair'
```

The leaf now goes into the transient cell decontainerized. Write-back is
unaffected: the caller stores that cell's post-call value into the source slot
itself.

## Pins

`t/vm/binding/rw-return-of-a-plain-value-is-refused.t` (16 assertions) and
`t/collections/range-pair/pair-reached-through-an-expression-is-immutable.t`
(14 assertions), each verified to fail without its fix and to pass under
rakudo v2026.07.
