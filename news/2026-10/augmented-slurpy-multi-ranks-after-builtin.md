# An augmented slurpy `multi` no longer beats the built-in method

`augment class Any { multi method map(|c) { ... } }` joins the built-in `map`'s
candidates in Rakudo and, being only a capture, ranks after the built-in's positional
candidate, so `(1, 2).map(* + 1)` still maps. mutsu ran the user candidate ahead of the
built-in for every call. The override gate (`native_lever_a_user_override_sym`) now
declines for a core-type receiver when every user candidate of the method is a `multi`
of only a capture or slurpies and the receiver has a built-in of that name at the user
candidates' level or below (`Mu.say` is wider than an `Any` augment and keeps losing to
it, as in Rakudo).

Not covered (filed as remainders on #12751): a user-class instance receiver
(`C.new.map(*.so)`) still takes the augmented candidate, and the built-in's candidates
have no signatures in the row table, so a call only the built-in's own catch-all accepts
(`map()` is ambiguous, `grep()` picks the user candidate in Rakudo) answers the
built-in's error. A user candidate with a positional parameter is also not ranked
against the built-in's.
