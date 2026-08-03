# An object hash loses its key type when bound to a `%` parameter

`my %h{Mu}` is an *object hash*: its keys are objects, canonicalised by `.WHICH`.
mutsu records that in two places — `HashData::key_type` on the value, and a
per-*variable-name* side table (`Interpreter::var_hash_key_constraints` plus the
`__mutsu_hash_key_type::<name>` env entry, see
`runtime_var_meta::var_hash_key_constraint`). Every subscript path in the VM asks
the **name-keyed** table, so an object hash that arrives under a different name
than the one it was declared with stops behaving like one.

A `%`-sigiled parameter is exactly that case:

```raku
my class S { }
my %o{Mu};
%o{S} = 7;
say %o.raku;              # (my Any %{Mu} = (S) => 7)   -- correct
sub f(%h) { say %h.raku }
f(%o);
```

```
raku:  (my Any %{Mu} = (S) => 7)
mutsu: (my Any % = "S|U553" => 7)
```

Inside `f`, `%h{S} = 1` takes the plain-hash store path, which stringifies the
key — and emits `Use of uninitialized value of type S in string context` for a
type object. The declaring variable is affected too (`%o.raku` afterwards also
reports a plain hash), so it is not merely a display problem.

## Where it bites

`Cro::HTTP::Router`'s `RouteSet::Handler`:

```raku
method !append-middleware(Supply $pipeline, @middleware, %connection-state --> Supply) {
    …
    my $cs-type = $comp.connection-state-type;
    with %connection-state{$cs-type} { … }
    else {
        my $cs = $cs-type.new;
        %connection-state{$cs-type} = $cs;
        …
    }
}
```

`%connection-state` is declared `my %connection-state{Mu}` in `invoke` and passed
by name into `!append-middleware`. Subtests 3 and 4 of the vendored Cro suite's
`http-middleware.rakutest` (`Cro::HTTP::Middleware::Conditional` and
`Cro::HTTP::Middleware::RequestResponse`) fail on this, drowned in
`Use of uninitialized value of type SkipPipelineState in string context`
warnings. Subtests 1 and 2 pass since
`news/2026-08/nested-whenever-emitter-ownership.md`.

## Shape of the fix

The value already carries the truth. `var_hash_key_constraint(name)` (and its
`_fast` twin) should fall back to the `HashData::key_type` of whatever `name` is
currently bound to, so a hash keeps its object-hash behaviour under any name.
That single fallback covers the ~15 VM call sites listed by
`git grep var_hash_key_constraint`, but each of those sites should be re-read
before trusting the fallback blindly — some pass the constraint on to a
re-canonicalisation step, and a hash that is *already* WHICH-keyed must not be
canonicalised twice.

Related, smaller: mutsu's `.raku` for an object hash prints `S => 7` where raku
prints `(S) => 7`.

## Reproducers

`tmp/objhash3.p6`, `tmp/objhash4.p6`, `tmp/objhash5.p6`.
