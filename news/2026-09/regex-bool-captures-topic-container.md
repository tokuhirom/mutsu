# Regex.Bool captures its defining scope's `$_` container

Boolifying a regex value (`?$re`, `so $re`, `if $re`, `$re.Bool`) matches it
against the `$_` of the scope the regex literal was written in. Rakudo captures
that `$_` *container*: a later assignment to it is seen, while a `for` loop or a
routine that binds its own `$_` is not.

PR #9390 (issue #9258) covered only half of that. A literal escaping a
*callable body* took a value snapshot of the frame's `$_`, and mainline literals
took nothing and fell back to whatever `$_` was visible at the boolification
site. So (issue #9396):

```raku
my $q = /zz/;
sub h { $_ = "zz"; $q.Bool }
$_ = "a";
say h();                          # raku False, mutsu True
for <zz> { say $q.Bool }          # raku False, mutsu True
sub g { my $r = /foo/; $_ = "foo"; $r.Bool }
say g();                          # raku True, mutsu False + a warning
```

Now every escaping regex literal (tail, `return`, assignment RHS, literal
element), whether in the mainline or in a callable body, captures the topic
through `Interpreter::topic_container_cell`. The first capture boxes the
env-held `$_` into a shared `ContainerRef` cell and rebinds `_` to it. An
assignment then writes through the cell (SetGlobal's existing write-through
path). A `for`/`given`/routine topic *binding* (`SetTopic`, a fresh frame)
installs a different value and leaves the captured cell alone. A `$_` held in a
local slot (a `sub f($_)` parameter) is still captured by value; it is
read-only there anyway.

SetGlobal's ContainerRef write-through branch returned before the `$_`-specific
side effects ran: the live write-back into `given $x`'s source variable and the
map rw-topic marker. With `$_` now possibly a cell, `given $t { my $g = /x/;
$_ = "no" }` stopped updating `$t`. Both side effects moved into helpers
(`write_topic_to_source_var`, `note_rw_map_topic`) that both branches call.

The `Compiler::in_callable_body` flag had no other use and was removed. Pinned
by `t/regex/regex-bool-lexical-topic.t`.
