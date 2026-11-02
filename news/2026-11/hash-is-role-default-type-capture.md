# `my %h is Role` binds the role's type-parameter defaults

`my %h is Q` (and `my @a is Q`) with `role Q[::T = Int]` ran `Q.new` with `T` bound to `Str`
(`new sees Str`), because the tie punned the bare role without its defaults. It now takes the same
pun `Q.new` does (`default_parametric_role_pun`), so `T` is `Int`. Hash::Sorted's
`t/01-basic.rakutest`, which builds a typed key array from `KeyT = str` in `new`, passes 21/21
(#15991).
