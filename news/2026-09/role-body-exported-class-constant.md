# `my class` / `my constant ... is export` in a role body are importable

A non-parameterized role body's `my class C is export` and `my constant X is export` are now
registered when the role is declared, like an exported `subset` or `enum` already was, so
`use Role` imports them without any class composing the role first (#9981). A lexical class's
export tags travel on its `__mutsu_export_type` marker. A parameterized role's class/constant
stay composition-only, since their bodies may name the role's type parameters.
