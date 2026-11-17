# A role body's `my constant` no longer leaks into the composing scope

A sigilless `my constant` declared in a role body stayed bound in the scope
that declared or composed the role, so `role R { my constant B = Mu.new };
class X does R {}; class B { ... }; B.new` reached the constant instead of
the class (#17833). The constant is the role body's lexical: once the
declaration-time evaluation or a composition (a class's `does`, a role
composed through another role, a runtime `does`/`but` mixin, a pun) has filed
its binding under the role's or the composing type's body lexicals, where the
role's methods read it, the scope's previous binding of the name and its term
key is put back.

A sigiled constant (`my constant %D`) still stays in the env, as a body
`my @x` does: the role body's subs reach it through that binding (#17854).
