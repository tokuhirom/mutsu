# A lazy AttrX::Mooish attribute round costs 9% fewer instructions

AttrX::Mooish's `t/090-concurrent*.rakutest` run 200,000 rounds of `Foo.new`,
the predicate, the lazy build and the clearer, and sit right at the ecosystem
sweep's 240 s budget (#17335). Four general changes take a round from 7.35M to
6.68M instructions (callgrind, release build):

- **Type checks of mixed-in and core objects are memoized.** The
  per-(class, type) memo only admitted instances of classes the program
  declared. A `.HOW` with a role mixed in (`$attr.package.HOW ~~
  Metamodel::GenericHOW`, four times a round), an `Attribute` with a role
  mixed in, a Capture, and a built-in scalar checked against a role's lexical
  `my class` each ran the full check, 25-65k instructions. A mixin is now
  keyed by its inner class and its markers, the only things its check reads;
  core catalog classes, meta-objects and metamodel names take part too.
  `$how ~~ Metamodel::GenericHOW` went from 65k to 20k instructions.
- **A Capture argument keys the method-call plan.** `$o.m($x, \( :builder ))`
  used to refuse the plan and the multi caches outright; a positional Capture
  is a plain value of class `Capture`, and a candidate that could look inside
  it already makes its candidate set uncached. 27.5k to 21.1k per call.
- **Class-body statics are injected from a prepared list.** Every method call
  re-seats its class body's `my` statics in the frame; the list is now built
  once per class and world generation instead of re-cloning and re-interning
  every name per call, and the method exit drops those names by symbol.
- **The exit-time attribute-rebind scan skips frames that only bound their
  own locals**, instead of comparing every `:=`-bound local with every
  attribute of the invocant.

The two test files still take most of the sweep budget on a 4-core runner;
#17335 tracks the rest.
