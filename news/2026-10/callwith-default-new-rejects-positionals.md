# `callwith` into the default constructor rejects positionals; statement prefixes expose `.blorst`

Two bounded pieces of #9761 (a user subclass of a RakuAST node class).

**The default constructor takes named arguments only, however it is reached.**
`class C { method new($x) { callwith($x) } }; C.new(1)` printed `C.new`: the
`callwith`/`nextwith`/`nextsame` leg that ends at the native `Mu.new` handed its
arguments straight to `bless`, which dropped the positional. rakudo dies with
`Default constructor for 'C' only takes named arguments`, exactly as a direct
`C.new(1)` does — and mutsu now raises the same `X::Constructor::Positional`
through the same check (`default_new_accepts_positionals`, shared by both
paths), so a builtin ancestor that does take positionals (`is Array`,
`is Version`) is unaffected.

**Statement prefixes name their payload.** A phaser node
(`RakuAST::StatementPrefix::Phaser::Leave.new(RakuAST::Block.new)`) and the
`do`/`try`/`gather` prefixes store their block-or-statement positionally;
rakudo exposes it as `.blorst`, and now so does mutsu.

What #9761 is really about — an instance of the user subclass carrying the
node its parent constructor builds — still needs a representation decision
against ADR-0011 and stays open.
