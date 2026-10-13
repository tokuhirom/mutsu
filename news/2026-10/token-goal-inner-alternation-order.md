Ratchet a token's inner alternatives before checking its `~` goal
----------------------------------------------------------------------

When a `token` uses `~`, its inner alternatives now follow source order and
commit before the goal is checked. A later alternative can no longer win only
because it matches more text before the goal. Rules retain their backtracking
behavior, and ordinary LTM alternatives keep their existing ranking.
