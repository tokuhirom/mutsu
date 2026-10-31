# `Plugin::Registry` names the declared class even when `Plugin` is an imported class

With `use A; use B;` where `A` exports `class Plugin` and `B` declares and
exports `class Plugin::Registry`, the bareword `Plugin::Registry` was rewritten
through the head `Plugin` (the imported class) and died with "Could not find
symbol '&Registry' in 'A::Plugin'". A qualified name that is itself a declared
class (or the class a `use` bound under that spelling) now resolves to that
class before the head-alias rewrite. Found via OO::Plugin: `use
OO::Plugin::Declarations` now loads; `use OO::Plugin` still stops at the slang
activation (`trait_mod:sym<for>` override, the #13370 refusal).
