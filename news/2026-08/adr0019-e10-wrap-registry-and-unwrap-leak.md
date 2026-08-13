# ADR-0019 E10: method wrap chains move into the registry, and `.restore()` stops lying

ADR-0019's Phase E box E10 asked for two things: move `method_wrap_chains` (the
side table backing `.wrap()` on a method *candidate* obtained via
`^lookup(...).candidates[N]`) into the canonical `Registry`, and delete the
`has_any_wrap_chains()` prefilter that disabled a hot dispatch cache for the
*entire program* the instant any method anywhere got wrapped. Doing the move
surfaced a real, previously-untested bug along the way: `.restore()` and
`.unwrap()` on a method-candidate wrap handle never actually removed the
wrapper.

## The program-wide cache gate

`vm_call_method_compiled_interpret.rs`'s hot dispatch path keeps a
`fast_method_cache` keyed by `(class, method)`, generation-checked against
`Registry::method_generation` so it clears automatically whenever a class,
role, or method table changes. Before this box, the cache read was also gated
on `!self.has_any_wrap_chains()` — a cheap `HashMap::is_empty()` check, but a
*global* one: if a single method anywhere in the whole program had ever been
wrapped, every other method call, wrapped or not, fell off the fast path and
paid the full resolve-and-dispatch machinery.

The gate is gone now. It turns out to be unnecessary once wrap mutations bump
`method_generation` like every other canonical dispatch write: a method is
only ever inserted into `fast_method_cache` after `check_method_wrap_chain`
has already returned `None` for it, so a wrapped method is never cached in the
first place, and wrapping a method that *was* cached evicts the stale entry
through the same generation bump that already clears the cache on any other
registry mutation. The other `has_any_wrap_chains()` call sites
(`class_dispatch.rs`, `ctor_phase_plan.rs`, `builtins_dispatch_next.rs`,
`check_method_wrap_chain` itself) guard a live `find_method_candidate_index`
scan, not a cache, so they stay — removing them would only add cost for
programs with no wraps at all.

## The leak

Confirming the design against Rakudo first surfaced a bug the design doc had
flagged but not diagnosed:

```raku
class Foo { method bar($x) { $x * 2 } }
my $inst = Foo.new;
my $wh = Foo.^lookup('bar').candidates[0].wrap(-> $self, $x { callsame() + 100 });
say $inst.bar(5);   # 110
$wh.restore;
say $inst.bar(5);   # raku: 10 -- mutsu (before this fix): 110
```

`.restore()`'s implementation only knew how to remove an entry keyed by
sub-id from the sub-level `wrap_chains` map. A method-candidate `WrapHandle`
never populated a meaningful `"sub-id"` — it held the candidate `Sub`'s own
identity, which has nothing to do with the `(class, method, candidate_idx)`
key its wrapper actually lived under in `method_wrap_chains` — so the lookup
always missed, `.restore()` silently did nothing, and still returned `True`.
`.unwrap($handle)` called directly on the candidate was worse: it never even
checked for the `^lookup` markers that `.wrap()` itself uses to detect a
candidate, so it fell straight through to sub-level logic and unconditionally
raised "not wrapped".

The fix gives a method-candidate `WrapHandle` its own attribute shape
(`wrap-class`/`wrap-method`/`wrap-candidate-idx`, plus the shared
`handle-id`), built by one helper shared between the `Sub`-from-`^lookup` wrap
site and the `Method`-instance wrap site (`.^methods(:local)`). `.restore()`
and `.unwrap()` now check for those attributes first and route through
`Registry::remove_method_wrap`/`pop_method_wrap`, which remove the entry and
bump `method_generation` in one step — the same mutation path `push_method_wrap`
already used, so there's exactly one place that knows how to touch this table.

Pin: `t/wrap-candidate-unwrap-restore.t`, 10 assertions verified against
Rakudo v2026.06 first, including the multi-candidate case.
