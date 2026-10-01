# `:ignoremark` runs on the compiled regex engine

The compiled regex engine (ADR-0135) declined every pattern that held a scoped `[:m …]` group, and a
whole-pattern `:m` asked for every end (`m:m:ex/…/`) always walked. Both now run compiled: the
group's body runs its own program over the mark-stripped subject and its ends are mapped back, the
way the walk has always matched it. A group whose body runs code or holds a backreference still
takes the walk.

Part of [#10255](https://github.com/tokuhirom/mutsu/issues/10255).
