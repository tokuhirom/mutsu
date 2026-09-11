# Four more rows off the parse-failure index: a tight `-->`, `.$name` on the topic, a pseudo-type positional, and a whole class of consumed-span panics

[#7954](https://github.com/tokuhirom/mutsu/issues/7954) is the located index of the
largest single `blocked_load` bucket in the first full-corpus ecosystem sweep — 56
distributions whose own modules do not parse, each reported only as "the parse
failed". Two of its rows landed earlier (`news/2026-09/regex-declarator-traits-and-stacked-loose-unary.md`).
This pass takes four more, three of them reduced from rows the index could only
mark as "containers" — the reported line named an enclosing `class`/`sub` because
the parser could not recover far enough to point at the real construct.

The method throughout: fetch the distribution's tarball from the URL in its
`ecosystem/dists/` record, `--dump-ast` each module the META6 `provides` names (a
parse error needs no dependency closure), then binary-search the failing file down
to the smallest line range that still fails, and reduce that to a one-liner checked
against `raku`.

## A `-->` written tight against a literal parameter

`Math::Matrix` selects among its `norm` candidates on a string literal, and writes
the return type with no space in front of it:

```raku
multi method norm(Math::Matrix:D: 'column-sum'--> Numeric) { ... }
```

A literal-value parameter is recognized by running the full expression parser over
the parameter text and asking whether what came back is a literal. Nothing stops
that parser's postfix layer from lexing the `--` of `-->` onto the literal it has
just read, so this arrived as `('column-sum'--) > Numeric` — not a literal, so the
literal-parameter branch declined, and the parameter list then failed outright. A
sigiled parameter (`$x-->`) was never affected: the parameter parser reads its name
itself and never reaches the postfix layer.

`parse_literal_param_value` now makes two passes. The broad pass is the old one, and
still recognizes every literal spelling mutsu supports. The narrow pass is the
literal-parameter grammar itself — an optional sign and one primary term, exactly
what `literal_value_from_expr` accepts — which structurally cannot reach an infix or
postfix operator, so the `-->` survives for the signature parser to read. Keeping the
broad pass first means nothing that parsed before parses differently now.

## `.$name` — an indirect method name on the topic

`python::itertools` writes `@iterable.map({.$function})`. The explicit-invocant form
(`$x.$function`) had a parser branch, and so did the code-sigil topic form (`.&name`),
but the `$`/`@`/`%` topic form had none, so `.$function` fell through to the
method-name parse and died. `topic_method_call` now mirrors the postfix path,
building the same `DynamicMethodCall` with the topic as invocant — a Code object is
invoked with it, a non-Callable is a runtime error, and `.&name` keeps its own
meaning.

## A pseudo-type as an anonymous positional

`CRDT` exports operators over its own type:

```raku
multi prefix:<-->(::?CLASS)  is export { X::G-Counter::Decrease.new.throw }
```

Only a `:` marker declares an invocant, so a parameter whose whole text is
`::?CLASS` or `::?ROLE` is an anonymous positional exactly as `sub f(Int)` is. The
parser instead treated *every* non-variable continuation of the pseudo-type as an
invocant, which made this a hard `X::Syntax::Signature::InvocantNotAllowed` — a
`sub` may not take an invocant — and, more quietly, turned `method m(::?CLASS)` into
a zero-argument method whose caller got "No such method". The branch now splits: a
following `:` (with or without intervening whitespace) is the invocant marker it
always was, and anything else is an anonymous typed positional, built by a new
`type_only_param` helper factored out of the identical bareword-type path so both
spellings pick up `is` traits and a `where` clause the same way.

## A whole class of consumed-span panics

`Collection`'s `RefreshPlugins.rakumod` did not report a parse error at all — it
**panicked**, on a byte index that fell inside a `｣`. The cause is a parser
invariant that a heredoc breaks. When a heredoc's introducing line carries code
*after* the marker:

```raku
MapFail.new(:note(qq:to/WARN/)).throw unless %released{$format}{$n-plug};
    Major part error? No released plugin ｢{ $n-plug }_v{ $n-v }｣ for ｢$plug｣
    WARN
```

the heredoc reader splices the rest of that line onto the text following the
terminator and resumes on that freshly built buffer. `rest` is then not a tail slice
of the string the caller passed, so the `&input[..input.len() - rest.len()]`
subtraction several parsers used to recover their consumed span named an arbitrary
offset — with ASCII text it silently read the wrong span, and with multi-byte text
it landed inside a character and aborted the process.

`consumed_span` already existed for precisely this, recovering the span by pointer
provenance and returning `None` when there is none; its doc comment even names the
heredoc case. Nine sites that predated it were still doing the subtraction, and two
of them were reachable: the statement-expression sink-warning wrappers (the
`Collection` panic) and the six fat-arrow autoquote decisions (`qq:to/E/ => 1`
panicked the same way). All nine now go through the helper; three further sites keep
the subtraction, because their `rest` provably comes from a scanner that cannot leave
the buffer (a digit scan, a name parse, a version-literal scan). The `None` fallbacks are
chosen per site: the sink-source wrappers have nothing to record without a span and
skip; the fat-arrow decisions only ever inspect the *start* of the span, so falling
back to the whole input answers them identically; the `for`-body `&?BLOCK` scan falls
back to the superset, which can only over-detect and so enables the block-magic
binding rather than dropping it.

## Result

`Collection`, `Math::Matrix` and `python::itertools` now parse every module in their
`provides` clean, and `CRDT`'s `G-Counter.rakumod` does too. Pins:
`t/routines/signature/literal-param-return-arrow.t`,
`t/routines/signature/signature-pseudo-type-anon-param.t`,
`t/oo/method/topic-dynamic-method-call.t`,
`t/lang/quoting/heredoc-trailing-code-consumed-span.t` — all four green under rakudo
itself, so they pin rakudo's behaviour rather than mutsu's.

`CRDT.rakumod` still does not parse, on one further construct the index had not
isolated: a type capture carrying a nominal invocant type, `method merge(::T CRDT:D: $ --> T)`.
That is not a parser-only gap — mutsu keeps a parameter's type constraint as a single
string, and the capture is encoded *in* that string, so there is no room for both
`::T` and `CRDT:D`; the plain `method m(::T: $x --> T)` form already mis-binds at
runtime. Filed as #7984 rather than forced through here, along with #7985 for a
narrower gap the pseudo-type fix exposed (`::?CLASS`/`::?ROLE` as a parameter type
inside a `role` never resolves to the consuming class). #7954 stays open: it is an
index, and the remaining rows span roughly twenty distinct constructs.
