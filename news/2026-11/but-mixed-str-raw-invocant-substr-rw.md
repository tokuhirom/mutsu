# A `but`-mixed Str: `substr-rw`, raw invocants and a role's `AT-POS`

`my $s = "asd" but StrIdx` with `method AT-POS(\SELF: $i) is raw {
SELF.substr-rw($i, 1) }` (the last block of `roast/S32-basics/xxPOS.t`) died:
`$s[1]` was "does not support associative indexing" and `substr-rw` was not
found on the mixin. Four gaps, one per layer:

- `substr-rw` is a `Str` row of the mutating method table; a `Mixin` over a
  `Str` now reaches it, and the splice keeps the mixin
  (`"abc" but R` stays `Str+{R}`).
- A role's methods never entered the class method table, so a raw invocant
  parameter among them (`\SELF:`) did not raise the ADR-0067 arrival flag; role
  registration now does.
- The raw-invocant oracles resolve a `Mixin` receiver through its composed
  roles, and the mixin dispatch binds the staged container for a raw
  parameter (it only did for an implicit `self`).
- `$s[i] = v` on a mixin whose role supplies a raw-invocant `AT-POS` boxes the
  variable before the call, so what the role hands back writes through.

`roast/S32-basics/xxPOS.t` passes and is whitelisted; its `TODO_roast` rows are
gone. Pinned by `t/regex/subst/but-mixed-str-raw-invocant-substr-rw.t`.
