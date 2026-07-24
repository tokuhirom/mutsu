# Positional parameters are matched against positional argument slots

Multi-dispatch's type check walked positional parameters against the *raw*
argument list, so a named argument written before a positional shifted every
positional parameter onto the wrong slot and no candidate matched:

```
Cannot resolve caller e(Int:D); none of these signatures matches:
    (Int $p, $y, $x)
```

Writing a named argument first is not an exotic style — it is exactly what
forwarding a capture after adding a named produces. `f(:x(1), |c)` puts `:x`
in slot 0 and the capture's positionals after it, which is the idiom
`OpenSSL::CryptTools` uses throughout:

```raku
multi encrypt(:$aes256! where .so, |c) { encrypt(:$cipher, |c) }
multi encrypt(Blob $plaintext, :$key, :$iv, :$cipher! where .so) { ... }
```

`args_match_param_types` now precomputes the indices of the positional
arguments and walks *those*: a plain positional parameter reads the slot its
own positional index names, and a variadic one (`*@a`, `|c`, a capture
subsignature) is handed the positionals from its index onward plus every named
argument. The `is rw` writability check and the argument-source lookup used for
typed-container matching follow the same index, so they no longer inspect a
neighbouring argument either.

The fix had been known for a while but could not land: with the indexing
corrected, `Test::Util`'s non-exported `our sub run(Str, Str, *%o)` — which
leaked into every consumer's scope — started matching `run(:out, "cmd")` and
beat the core builtin. That leak is gone (news
`2026-07/unit-module-routines-stay-in-their-package.md`), so this lands on its
own merits.

The bundled OpenSSL battery's `t/04-crypt.rakutest` goes from 1/13 to 6/13 —
every `encrypt`/`decrypt` call in it goes through that forwarding chain. The
remaining failures are a separate NativeCall bug (a `Blob` argument passed to a
native sub is written back into, so the caller's immutable Blob is clobbered),
recorded in `PLAN.md`.

Pinned by `t/multi-dispatch-named-before-positional.t`, which passes
identically under `raku`.
