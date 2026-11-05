# A closure keeps the `::T` type capture of the routine that created it

`sub mk(::T) { -> { T.^name } }` called under another routine that has its own `::T`
(`sub call(::T, &f) { f() }`) printed the CALLER's type: the bare word `T` was resolved by
name at call time, and a type name never counted as a captured lexical. A nested closure
now reads an enclosing routine's type capture as a by-name free variable, and the creating
frame vouches for it (a type capture is bound once and never reassigned), so the captured
type wins over the caller's binding as any other captured lexical does.

Found with the Deps distribution: `Deps.register: C2` stores a factory building `Type.new`,
and `Deps.get(R)` (R a role C2 does) ran it inside `get(::Type)`, so it built `R.new`. Deps'
`t/01-basic.rakutest` goes from 9 to 30 passing assertions.
