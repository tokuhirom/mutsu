# Subscript assignment through a *scalar* attribute is silently dropped

`$!attr<key> = v` and `$!attr[0] = v`, where `$!attr` is a `$`-sigil attribute
holding a Hash or Array, do not persist. The `%!`/`@!` sigil forms work, and the
same write to a *lexical* `$` holding a Hash works, so this is specific to the
scalar-attribute path.

## Repro

```raku
class A {
    has $.h = {};
    has $.a = [];
    has %.hh;
    has @.aa;
    method fill() {
        $!h<k>  = 1;
        $!a[0]  = 2;
        %!hh<k> = 3;
        @!aa[0] = 4;
    }
}
my $o = A.new; $o.fill;
say $o.h.raku;    # raku: {:k(1)}   mutsu: {}
say $o.a.raku;    # raku: [2]       mutsu: []
say $o.hh.raku;   # both: {:k(3)}
say $o.aa.raku;   # both: [4]
```

The write is lost silently — no error, no warning.

## What it is NOT

Narrowed with variants; all of these still fail, so none of them is the trigger:

- typed (`has Hash $.h`), untyped (`has $.h = {}`), or a user Hash subclass
  (`class M is Hash {}`, `has M $.h`)
- assignment (`=`) or `||=`
- written from `BUILD`, from `TWEAK`, or from an ordinary method
- attribute defaulted in the declaration or bound through a `BUILD` named
  parameter

And these **work**, which localises it:

- `my M $s .= new; $s<x> = 1` — a lexical scalar holding a Hash subclass
- `%!hh<k> = 3` / `@!aa[0] = 4` — the `%`/`@` sigil attribute forms

So the scalar-attribute lvalue path is not writing back through to the
attribute's container the way the sigilled ones do.

## Impact

Found as the cause of `Template::Mustache`'s `06-logging.rakutest`
(`todo/tickets/mustache-remaining-two-files.md`): its `Logger` declares
`has LoggersMap $.routines` (a `Hash` subclass) and fills it in `BUILD` with
`$!routines{.key} ||= …`. Under mutsu the map comes out **empty**, so every log
level is unset and replacing `routines<Warn>` with `&die` has no effect.

The construct is ordinary Raku, so the blast radius is much wider than that one
module — any class that keeps a hash or array in a `$`-sigil attribute and
subscripts it is affected.
