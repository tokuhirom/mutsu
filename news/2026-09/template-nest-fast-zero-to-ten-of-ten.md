# Template::Nest::Fast goes 0/10 → 10/10, and none of the five bugs were in the template engine

`Template::Nest::Fast` 0.3.0 was the last "cheap whole row" in
`todo/deep/template-engines-blocked-on-mutsu.md`: healthy under raku (10/10),
0/10 under mutsu. It is now **10/10**, from five general interpreter fixes. As
with `Template6` before it, every one of them was found by reducing a divergence
to a two-line snippet, and not one of them was in the code the ticket pointed at.

## The two fixes that got it off zero (2/10)

Landed a few hours earlier, in a separate change:

- `with EXPR -> @m { }` bound `@m` to a **one-element wrapper** containing the
  list instead of to the list itself, because the desugaring temp tripped the
  compiler's `my @a = $x` itemize rule. `!index-template`'s
  `with $f ~~ m:g/…/ -> @m` therefore saw one match, and `$m[0].from` was `Nil`.
- `can-ok` did not see auto-generated attribute accessors while `$obj.can('x')`
  did — two paths answering the same question differently.

## Splicing: an lvalue method's own arguments kept their element containers

`render` splices with `$rendered.substr-rw(%v<start-pos>, %v<length>) = $append`.
Under mutsu the *length* was ignored whenever it was not an integer literal, so
each splice replaced the whole tail of the template:

```raku
my $l = 3;
my $s = "hello";
$s.substr-rw(1, $l) = "Z";
say $s;             # raku: hZo    mutsu (before): hZ
```

Root cause: `$o.meth(args) = v` lowers to `__mutsu_assign_method_lvalue`, whose
whole argument list is exempt from the call-site auto-FETCH because the
**invocant** is a container by contract (ADR-0040 §9). The assigned *value* was
re-fetched explicitly; the method's own arguments were not. They arrive inside
an `ArrayLiteral` carrier, and ADR-0040 makes an `Array` element a `Scalar`
container at the store — so a **variable** argument arrived as a `ContainerRef`
while a **literal** one arrived bare. Every consumer then had to decide for
itself: `substr_resolve_position` happened to stringify-and-parse its way out of
it (which is why the *start* position worked by accident), while
`resolve_substr_rw_range`'s length arm and `substr_extract_range` both fell to
their "no length given" / "not a Range" defaults.

The fix reads through the container once, in
`builtin_assign_method_lvalue`, where the carrier is unpacked — the method's own
arguments are ordinary rvalues, exactly like the assigned value.

A neighbouring divergence fell out of the same reduction and is fixed with it: a
`$`-held Range is itemized, so `my $r = 1..3; $s.substr($r)` handed `substr` a
`Scalar`-wrapped Range and it silently took the whole rest of the string.
`substr_extract_range` and `substr_resolve_position` now descalarize first.

Pin: `t/substr-rw-computed-args.t`.

## A `%`/`@` loop parameter was exempt from the loop's save/restore

With the splice fixed, the output was still wrong — but now *correctly spliced
at the wrong offsets*: the parent template got the nested component's `start-pos`
and `length`. Instrumenting the dist showed `render`'s `for @(%t-indexed<vars>)
-> %v` loop resuming its **outer** iteration with the **inner** recursive
frame's `%v`:

```raku
class Recurse {
    method walk(@vars, $depth --> Str) {
        my Str $acc = '';
        for @vars -> %v {
            my Str $inner = $depth == 0 ?? self.walk([%(n => 'I1')], 1) !! 'x';
            $acc ~= "[{%v<n>}:$inner]";
        }
        return $acc;
    }
}
say Recurse.new.walk([%(n => 'O1'), %(n => 'O2')], 0);
# raku:           [O1:[I1:x]][O2:[I1:x]]
# mutsu (before): [I1:[I1:x]][I1:[I1:x]]
```

The VM saves and restores a single named `for` parameter's prior binding so a
nested loop over the same name cannot leak out — but it explicitly skipped `@`
and `%` sigils, "which bind a shared mutable container the body may legitimately
reassign". That reasoning does not hold: a pointy parameter's scope ends with the
loop either way, and restoring the **name** does not undo a mutation of the
container the name pointed at (same Gc node before and after). What the exemption
actually cost was recursion — the same `for` re-entered from a nested frame left
the inner frame's last element bound on return.

`@`/`%` parameters are saved and restored now, in all three places that had to
agree so the deferred-restore push/pop stay balanced: `vm_for_loop_body.rs`,
`vm_for_loop_intrange.rs`, and the two compiler sites that emit
`OpCode::RestoreForParam`. Aliasing is unchanged — `for @aoa -> @row {
@row.push(9) }` still writes through to the source element.

Pin: `t/for-container-param-recursion.t`.

## `IO::Path.modified` was truncated to whole seconds

The last file, `09-advanced-indexing`, exercises the dist's re-index check:
`%t-indexed<path>.modified > %t-indexed<modified>`. It never fired, because
mutsu's `.created`/`.modified`/`.accessed`/`.changed` built their `Instant` from
`Duration::as_secs()` — so two writes inside the same second compared equal.

```raku
my $p = "x".IO; $p.spurt('one'); my $a = $p.modified;
sleep 0.05;     $p.spurt('two'); my $b = $p.modified;
say $b > $a;    # raku: True     mutsu (before): False
```

raku reports these to nanosecond resolution. They now keep the sub-second part
(`as_secs_f64`, and `ctime` + `ctime_nsec` for `.changed` on unix).

Pin: `t/io-path-timestamp-subsecond.t`.

## Why this keeps happening

Three for three, on the three template dists reduced so far, the recorded
"first failure" was a pointer and not a diagnosis, and the actual causes were in
subsystems the ticket never mentioned — the regex tokenizer for `Template6`, the
lvalue-argument boundary and the for-loop parameter lifetime here. The method in
`todo/deep/template-engines-blocked-on-mutsu.md` — reduce by deletion until a
two-line repro falls out, and check every reduction against raku — is what found
all of them. Reading the first error line found none.
