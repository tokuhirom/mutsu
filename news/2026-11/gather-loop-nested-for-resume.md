# A lazily pulled `gather loop`/`while` resumes correctly after a nested `for` suspends

When a `for` nested in a `loop`/`while`/`until` inside a lazily pulled `gather` hit the pull's
take limit, the enclosing loop ran its step (and then re-checked its condition) before the `for`
continued, so elements were skipped or the wrong iteration resumed
(`gather loop (...) { for inner() -> $x { take $x } }.map({$_})` lost elements). The loop now
re-enters the interrupted iteration's body directly. This was the real cause behind the
"recursive gather with `given`" wrong answers of #16855 (Math::Sequences `strict-partitions`).
