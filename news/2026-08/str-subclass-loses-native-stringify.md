# A user class `is Str` inherits the native string payload

`Language/objects.rakudoc:1067` showed a user subclass of a native type losing the
parent's representation entirely:

```raku
class Foo is Str { }
my $x = Foo.new(:value("hi"));
say $x; say $x.Str; say "v=$x";
```

raku prints `hi` / `hi` / `v=hi`; mutsu printed `Foo.new` / `Foo.new` / `v=Foo()`,
and `$x.chars` was `5` (the length of the `Foo()` placeholder), `$x.uc` was
`FOO()` and `$x eq "hi"` was `False`.

## What the construction spelling actually is

Worth confirming before implementing, since the ticket's repro could have been
mis-specified: it is not. Rakudo's `Str` carries exactly one attribute — a
`BOOTSTRAPATTR` named `value` (`Str.^attributes.raku` → `(BOOTSTRAPATTR.new,)`) —
and `Mu.new` fills it from the `:value` named argument. `Foo.new("hi")` is *not*
the spelling: it dies with `Default constructor for 'Foo' only takes named
arguments`, in Rakudo as in mutsu. `Foo.new` with no arguments gives the empty
string (`.chars` is `0`, `.raku` is `""`).

## Root cause and fix

mutsu had no notion of the payload. `Foo.new(:value("hi"))` went through the
ordinary named-arg → attribute mapping, which stores a named argument only when
`is_attribute_buildable` says the class declares it — and `Foo` declares nothing —
so the string was dropped on the floor and every stringification path fell through
to the generic `Class.new` / `Class()` rendering.

The precedent for this already existed twice over: an `is Array`/`is List`
subclass gets a reserved `__mutsu_array_storage` attribute, an `is Hash`/`is Map`
one gets `__mutsu_hash_storage`, and an `is Int` one gets `__mutsu_int_value`. The
fix is the string twin, `__mutsu_str_value`, set in the same block of
`methods_object_dispatch_new.rs` when the class MRO contains `Str`, read from the
`:value` named argument (defaulting to `""`). Storing it as a reserved attribute
rather than consulting the class registry means the `Value` layer can find it
without a registry lookup, which is what makes the cheap paths work.

Three consumers were then enough:

- `Value::to_string_value` (`src/value/display.rs`) returns the payload instead of
  `Foo()`. Because the native `Str` methods reach the receiver through this,
  `.chars`, `.uc`, `eq` and `qq` interpolation all start working from this one
  arm — they were never separately broken, they were reading the placeholder.
- `gist_value` returns the payload, so `say $x` prints `hi`. (`Str.gist` is the
  string itself, so this is not merely a delegation to `.Str`.)
- The `raku`/`gist` instance arm in `methods_instance_ops.rs` delegates to the
  payload value, giving `$x.raku` → `"hi"`, mirroring the `is Array`/`is Hash`
  delegation immediately below it.

`$x.^name` stays `Foo` and `Foo.^mro` still contains `Str`, so nothing about the
class identity changes.

Pinned by `t/str-coercion-and-dispatch.t`.
