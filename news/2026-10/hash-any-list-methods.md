# `Hash`/`Map` and `Set`/`Bag`/`Mix` get `Any`'s list methods: `reverse`, `unique`, `squish`, `Seq`, `eager`, `Supply`, `minmax`, `produce`

Raku's `Hash`/`Map` and the QuantHashes inherit these methods from `Any`, which defines each as
`self.list.METHOD`: on such a receiver the invocant *is* its list of Pairs. mutsu treated the hash
as ONE opaque item, or (for `reverse`) had no such method at all, even for a plain `%h`:

```raku
my %s = a => 1;
say %s.reverse.raku;   # rakudo: (:a(1),)       mutsu before: No such method 'reverse' for invocant of type 'Hash'
say %s.unique.raku;    # rakudo: (:a(1),)       mutsu before: ${:a(1)}
say %s.squish.raku;    # rakudo: (:a(1),)       mutsu before: ({:a(1)},)
say %s.Seq.raku;       # rakudo: (:a(1),).Seq   mutsu before: ({:a(1)},)
say %s.eager.raku;     # rakudo: (:a(1),)       mutsu before: ({:a(1)},)
say %s.Supply.list.raku;  # rakudo: (:a(1),)    mutsu before: ({:a(1)},)
my %t = a => 1, b => 2;
say %t.minmax.raku;    # rakudo: :a(1)..:b(2)   mutsu before: {:a(1), :b(2)}..{:a(1), :b(2)}
say bag(<a a b>).reverse.sort.raku;   # (:a(2), :b(1)).Seq -- mutsu before: No such method 'reverse' for invocant of type 'Bag'
```

(The first three of those, and `tail`/`rotor`/`cache`/`permutations`, were also wrong for a
`$`-held hash; the itemized half landed in [#10769](https://github.com/tokuhirom/mutsu/pull/10769).
This is the part that is wrong for a bare `%h` too.)

Each of these methods had its own `match target.view()` with arms for Array/Seq/Slip/Range and a
fallback that either returned the receiver as it was (`unique`), wrapped it in a one-element list
(`squish`, `eager`, `Seq`, `Supply`), or declined (`reverse`). There is now one place that knows the
rule: `utils::hashlike_receiver_as_pairs_list` returns the receiver's list of Pairs for a listed
method on a `Hash`/`Map`/`Set`/`Bag`/`Mix`, and both dispatch entries re-dispatch the same method on it
-- `Interpreter::call_method_with_values` (which serves the methods that take arguments, such as
`minmax(&by)` and `produce(&code)`, and lets a user `augment` of the same name win) and
`native_method_0arg` (the one entry for every zero-argument native method, reached from every VM call
op). `.Seq` has a single shared implementation, `seq_coerce::to_seq_structural`, so its Hash/Set/Bag/Mix
arm lives there instead. The receiver's own itemization is ignored and an object hash keeps its key
objects, because the Pairs come from `value_to_list_for_receiver`.

`duckmap`, `deepmap` and `nodemap` on a Hash map over the *values* and rebuild a real Hash, but stored
each result raw, so a Boolean block result rendered with the Pair shorthand (`{:a}`) instead of the
long form a Hash element store gives (`{:a(Bool::True)}`). They now go through
`Value::itemize_for_hash_element`, the same hook the element stores use.

Pinned by `t/collections/hash/hash-any-list-methods.t` (36 assertions, every one also checked against
`raku`; multi-key results are sorted because hash order is arbitrary). Closes #10758.
