# A `*.method` value keeps its braces a hash composer

`{ :status(*.so) }` composed a `Block`, not a `Hash`. The brace-disambiguation
scan in `src/parser/primary/misc/lambda.rs` treats an invocant-less method call
as a topic reference — `{ a => .key }` really is a block — and it decided
"invocant-less" by looking at the byte before the dot. A `*` was not in the list
of things a term can end with, so the `.so` of `*.so` read as `$_.so` and forced
the block branch. Any `*.method` inside braces was affected: `{ s => *.abs }`,
`{ :err(/Sub/), :status(*.so) }`, and so on.

The star is genuinely ambiguous from that position, because infix
multiplication is spelled the same way: `{ a => 2 * .elems }` *is* a block, and
its `.elems` *is* on the topic. So the fix does not simply add `*` to the
term-enders — it walks back past the star (and past a `**` HyperWhatever) and
asks what precedes it. A term there means the star is an infix and the call is a
topic call; anything else — `(`, `,`, `=>`, the start of the body — means the
star is a Whatever and is itself the invocant.

Found under the real `Test` module. `roast/S24-testing/11-plan-skip-all-subtests.t`
passes `{:err(/Sub/), :status(*.so)}` to `Test::Util`'s `is_run`, and with the
hash arriving as a `Block` the multi-dispatch picked the no-test-name candidate,
so the assertion lost its description and, because that candidate's `ok` fell
through to mutsu's native provider, the file's own counter stopped at 2 of 4.
The file now matches `raku` byte for byte. Pin: `t/hash-literal-whatever-value.t`.
