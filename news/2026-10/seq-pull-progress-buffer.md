# Every deferred map/grep resumes after a failed read

A deferred `Seq` whose callback dies during a non-consuming read keeps the
elements it produced and resumes after the failing element, as rakudo's
`.cache` does. Since #12048 only the inline `map` and `grep` loops and the
`Iterator` pull did this. A chained `map`/`grep`, an rw `map`, a map with a
typed parameter or a routine callback, and a prefix read such as `$s[1]` still
left the `Seq` consumed: the next read died with "The iterator of this Seq is
already in use/consumed".

The progress used to travel on the exception (`SeqPullProgress`), so each loop
had to remember to attach it. Now every pull fills an output buffer that its
caller owns (`PullOut { produced, consumed }`), element by element, so the
caller already holds the progress when the callback dies. The exception
side-channel is gone (#12401; ADR-0034, 2026-10-20 amendment).
