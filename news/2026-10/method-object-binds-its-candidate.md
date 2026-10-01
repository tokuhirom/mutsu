# A Method object invokes its own candidate

Invoking a native or attribute-accessor `Method` object
(`D.^lookup('x')($e)`, `D.^methods.first(*.name eq 'x')($e)`, `$e.$m()`)
used to re-resolve the method by *name* on the invocant, so a subclass
override won: with `class E is D { method x { 99 } }`, `D.^lookup('x')($e)`
answered `99` instead of `D`'s attribute. Assigning through such an object
(`D.^lookup('x')($e) = 5`, `$e.$m() = 5`) likewise wrote through the
override. The object now binds to its declaring candidate by dispatching
under its owner-qualified name (`$e.D::x`), exactly as Rakudo does; a
`.^method_table` entry is bound the same way.

The qualified call itself (`$e.D::x`) now runs a wrap chain installed with
`.wrap` on the accessor's Method object, sharing the chain lookup with
ordinary dispatch, and a qualified lvalue assignment commits into the
instance's shared attribute cell even when there is no caller variable to
rebind. Qualified lvalues also split `A::B::x` at the last `::`, so a
nested-package owner resolves.
