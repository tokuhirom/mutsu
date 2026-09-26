# Hyper operators over Set/Bag/Mix operands hyper over their keys

A hyper operator whose operands include a Set, Bag or Mix now treats that
operand as the Associative it is, as rakudo does. Before, mutsu walked the
QuantHash as a list of Pairs and nested the results:
`set(1,2) «∪» set(3)` answered `(1 => 3 => Set.new(True), ...)` instead of an
empty Set.

- **Keys.** The dwim arrows pick the key set the same way they do for two
  Hashes: union, intersection, or the left or right keys.
- **Missing keys.** A key that a QuantHash lacks reads as its absent weight:
  0 for a Bag or Mix, `False` for a Set.
- **Result type.** The result takes the type of the operand that donates the
  structure. That is the left operand when it is Associative, otherwise the
  right one. So `bag(...) »+« %h` is a Bag, `%h »+« bag(...)` is a Hash, and
  `1 «+« bag(...)` is a Bag.
- **Nesting.** This holds at every depth: `(set(1),) «∪» (set(3),)` works too.
- **Typed elements.** Elements that are not strings keep their type.

A QuantHash operand is projected through its `.Hash` coercion, which is the one
implementation in `builtins::map_hash_coerce::to_hash`. The two-Hash hyper
became `hyper_hash_pair`, which takes the missing value for each side as an
argument, and the QuantHash path shares it. The QuantHash helpers moved to
`src/vm/vm_hyper_quanthash.rs`, and the `»[&f]«` form uses the same helpers.

A related gap remains for plain Hashes. A missing plain-Hash key still reads
as the operator's identity, where rakudo reads `Any`. That is tracked in #9564.

Test: `t/lang/operators/hyper-quanthash-operands.t`. Closes #9482.

The integer bitwise operators also read a `Bool` held in a hash or array
element correctly now. Before, `%h<a> +| %h<a>` with `%h<a> = True` was 0,
because `int_operand` did not see through the Scalar that holds the element.
The Set hyper path reached that bug through `»[&infix:<+|>]«`. Test:
`t/types/numeric/bitwise-bool-scalar-operand.t`.
