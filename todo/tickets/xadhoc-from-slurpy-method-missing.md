# `X::AdHoc.from-slurpy(...)` class method is entirely unimplemented

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/X/AdHoc.rakudoc:56`).

## Root cause

`X::AdHoc.from-slurpy(...)` is a documented class method that builds an `X::AdHoc`
whose `.payload` is a `Capture+{X::AdHoc::SlurpySentry}` built from the positional
slurpy arguments, and whose `.message` is the concatenation of their stringifications.
mutsu has no such method at all — calling it throws `X::Method::NotFound`.

## Minimal repro

```raku
my $e = X::AdHoc.from-slurpy(3, False, "x");
```

- `raku`: succeeds, `$e.payload.^name` is `Capture+{X::AdHoc::SlurpySentry}`,
  `$e.message` is `3FalseNot here`-shaped concatenation.
- `mutsu` (`target/debug/mutsu`): `No such method 'from-slurpy' for invocant of type
  'X::AdHoc'`.

Doc's fuller example:

```raku
try {
    X::AdHoc.from-slurpy( 3, False, "Not here" ).throw
};
print $!.payload.^name; # Capture+{X::AdHoc::SlurpySentry}
print $!.message;       # 3FalseNot here
```

## Affected files (starting point)

`X::AdHoc` class-method registration — grep for where `X::AdHoc`'s other methods
(`.new`, `.message`, `.payload`) are implemented (likely `runtime/class.rs` /
`runtime/methods.rs` exception-type bootstrapping) and add a `from-slurpy` class
method alongside them.
