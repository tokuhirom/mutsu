# A `Proxy` returned from `is rw` `AT-POS` loses its STORE, and its capture loses to a same-named outer lexical

Two separate defects in the same shape — an `is rw` `AT-POS` that returns a `Proxy`
closing over a routine-local lexical. Both are **name-independent**: they reproduce with
an ordinary variable name, so neither is the `my $self` / invocant collision fixed by
ADR-0061 (they were found while verifying that fix and survived it unchanged).

## Defect 1 — STORE through the Proxy does not persist

```raku
class B {
    has @.nodes;
    method AT-POS($offset) is rw {
        my $slf = self;
        Proxy.new(
            FETCH => method () { $slf.nodes[$offset] },
            STORE => method ($val) { $slf.nodes[$offset] = $val }
        )
    }
}
my $b = B.new(nodes => ['x','y']);
say $b[1];        # raku: y    mutsu: y   (FETCH is fine)
$b[0] = 'z';
say $b[0];        # raku: z    mutsu: x   <-- the STORE was a no-op
```

The FETCH half works, so the Proxy is reached and its closure is intact; the assignment
`$b[0] = 'z'` simply does not route through the container's `STORE`. Candidate area: the
element-assignment path decides whether the subscript target is a container before it
knows the `is rw` `AT-POS` returned one — compare
[ADR-0059](../../docs/adr/0059-is-rw-routines-return-a-container.md) (an `is rw` routine
returns a container), whose Slices 2-3 are still open.

## Defect 2 — a same-named outer lexical breaks the deferred capture entirely

Add a mainline declaration of the *same name* the method-local uses, and both halves
collapse to `Nil`:

```raku
my $slf = 1;                 # <-- the only difference
class B { ...as above... }
my $b = B.new(nodes => ['x','y']);
say $b[1];                   # raku: y    mutsu: Nil
```

The `Proxy`'s `FETCH` method literal is created inside `AT-POS` and called much later,
after that frame is gone. When an outer scope has a lexical of the same name, the
deferred closure resolves to it (or to nothing) instead of to the routine-local binding it
closed over. Note that the equivalent *non-deferred* shape is fine —

```raku
my $z = 1;
class D { method make-cb { my $z = self; method () { $z.^name } } }
say D.new.make-cb()(D.new);   # mutsu: D — correct
```

— so it is specific to the capture surviving into a `Proxy` that is invoked from the
container-read path, not to same-name shadowing in general. Related: ADR-0055 (a
closure's free variable resolves to its own binding), which is still `Proposed`.

## Why these are tickets, not part of ADR-0061

ADR-0061 fixed the `$self`-specific collision: `my $self = self` + `Proxy` no longer
overflows the stack, and `t/lexical-self-vs-invocant.t` pins the read path for both
`AT-POS` and `AT-KEY`. That test deliberately does **not** assert the write-back or the
shadowed-outer variant, because both fail for reasons that have nothing to do with the
name `self`. Fixing either would make those assertions addable to the same test file.

## Repro

```sh
cargo build
# Defect 1
timeout 20 ./target/debug/mutsu ./tmp/proxy-store.raku
# Defect 2: prepend `my $slf = 1;` to the same file
```
