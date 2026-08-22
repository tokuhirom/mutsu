# A grammar-embedded custom assertion method (`<.method>`) sees `self` as an uninstantiated type object

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/grammars.rakudoc:387`).

## Root cause hypothesis

A grammar can define its own methods (not via a separate `actions =>` class) and call them
as custom regex assertions with `<.method-name>`. Inside such a method, `self` should be
the in-progress grammar instance (so attribute reads/writes like `$!invalid = True` work,
and returning `self` is the idiomatic way to signal "accept"/"error" while continuing the
parse). On mutsu, invoking one of these embedded methods via `<.method>` treats `self` as
the grammar's bare **type object** rather than an instance — attribute assignment fails
with `Cannot look up attributes in a G type object. Did you forget a '.new'?`, and even
when the method has no attributes at all (just `self;`), the overall `.parse()` result is
`Nil` instead of a `Match`.

This is a different code path from the ordinary subrule/token dispatch (`<name>` calling
another `token`/`rule`) and from `actions=>`-class method dispatch (`<sym>`/`make`) — both
of those already work correctly in mutsu's grammar engine.

## Minimal repro

```raku
grammar G {
    token TOP { <a> }
    token a { \w+ <.mark> }
    method mark() {
        self;
    }
}
say G.parse("hello").WHAT;
```

- `raku`: `(G)`
- `mutsu`: `Nil`

With an attribute involved (closer to the original doc example), it's a hard error instead
of a silent `Nil`:

```raku
grammar G {
    has Bool $.invalid;
    token TOP { <a> }
    token a { \w+ <.mark> }
    method mark(--> ::?CLASS:D) {
        $!invalid = True;
        self;
    }
}
my $m = G.parse("hello");
say $m.WHAT;
```

- `raku`: `(G)`
- `mutsu`: `Cannot look up attributes in a G type object. Did you forget a '.new'?`
  (`in sub mark at ... line 1`)

## Affected files (starting point)

- `src/runtime/regex/` — the custom-assertion dispatch for `<.method-name>` (distinct from
  ordinary subrule-token dispatch and from `actions=>` dispatch); look for wherever a
  grammar method is called as a regex assertion and how `self` is bound for that call —
  it needs to resolve to the same in-progress instance the enclosing `token`/`rule` body
  sees (`$/`'s originating grammar object), not the class/type object.
- `src/runtime/methods_grammar.rs` may be the right place to compare against the working
  subrule/actions dispatch paths.
