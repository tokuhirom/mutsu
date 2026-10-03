# A `proto method` passed to a user `trait_mod:<is>` is a dispatcher

The `Method` handed to a user `trait_mod:<is>` for a `proto method` (class or role body)
now answers `.is_dispatcher` True, as in Rakudo; `multi method` candidates still answer
False. Found by working the `Method::Also` distribution, whose role-level alias hook skips
any method that is not a dispatcher. Its last two test assertions still need role body HOWs
to be `ParametricRoleHOW` and the role `specialize` hook (#10466).
