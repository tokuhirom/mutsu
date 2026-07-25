# `Grammar.parse` fails to dispatch inside the full `YAMLish` module

Blocker #2 for the `YAMLish` battery candidate (`docs/batteries/yaml.md`), and
it is **not yet root-caused**.

After the module-load blocker
(`todo/tickets/whatever-curry-through-fatarrow.md`) is patched past locally,
`load-yaml` reaches:

```raku
our sub load-yaml(Str $input, ...) is export {
    my $match = Grammar.parse($input);   # lib/YAMLish.rakumod:944
    ...
}
```

and dies with:

```
Couldn't parse YAML: X::Method::NotFound: Unknown method value dispatch (fallback disabled): parse
```

## Not the obvious hypothesis

The tempting explanation — "the user declared `grammar Grammar {…}`, which
shadows the built-in `Grammar` type, and mutsu resolves the name to the core
type object that has no `.parse`" — is **wrong**, or at least incomplete. An
isolated repro works:

```raku
grammar Grammar { token TOP { \d+ } }
say Grammar.parse("123");   # mutsu: 「123」 — dispatches fine
```

So the failure is **context-dependent**. `YAMLish` defines four grammars with
inheritance — `Grammar` (lib/YAMLish.rakumod:150), `Schema::JSON` (784),
`Schema::Core is Schema::JSON` (839), `Schema::Extra is Schema::Core` (885) —
plus heavy inline `{ make … }` actions and a large token set. The dispatch
failure needs reduction against that fuller context before a fix.

Note the error text `Unknown method value dispatch (fallback disabled)` is a
**recurring mutsu pattern** — the same shape blocks `Template::Classic` in the
template survey (`docs/batteries/templates.md`) — so root-causing it may pay off
beyond YAML.

## How to reproduce

The whole module is load-blocked by bug #1, so to see this bug today you must
first get past the load: take `lib/YAMLish.rakumod`, replace the `flatten-tags`
body (line 939) with an explicit two-arg-block equivalent:

```raku
return %tags.kv.map(-> $ns, %v { |%v.kv.map(-> $k, $val { ($ns ~ $k) => $val }) });
```

then:

```sh
echo 'use YAMLish; say load-yaml("foo: 1");' \
  | mutsu -I <patched-lib> -I modules/MIME-Base64/lib -
# -> Couldn't parse YAML: X::Method::NotFound: … parse
```

Once bug #1 is fixed in the interpreter, the patch is unnecessary and this
reproduces directly on the vendored module.

## Next step

Reduce: define `Grammar` alongside a `Schema::JSON`/`Schema::Core` inheritance
chain and inline actions, and bisect toward the minimal shape that makes
`Grammar.parse` mis-dispatch. Do **not** assume the name-shadow theory without a
reducing case that actually fails.
