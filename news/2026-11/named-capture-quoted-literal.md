# `$<k>='run'` aliases the whole quoted literal

A named-capture alias on a multi-character quoted literal (`$<key>='run'`)
bound only the first character: the literal lowers to one token per character
and the alias landed on the first. The alias now goes on a group around the
quoted atom, as in Rakudo (a bare word still aliases only its first
character). Jupyter::Kernel's `%% run FILE` magic parsed to `Nil` because of it.
