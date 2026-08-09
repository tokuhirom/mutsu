# grep/map no longer overwrite `$_` for blocks with an explicit signature — http-cookiejar 30/30

A pointy block with an explicit signature (`-> $c { ... }`) does not declare
`$_`; a `$_` in its body is a lexical reference to the enclosing scope's
topic. mutsu's inline map/grep/first fast paths instead re-bound `$_` to the
iteration element on every call, so `for <a b> { <a b c>.grep(-> $c { $c eq
$_ }) }` compared each element with itself (always true) instead of with the
`for` topic.

The fix generalizes `whatever_code_keeps_outer_topic` into
`block_keeps_outer_topic` (`src/runtime/resolution_map_grep.rs`): any block
whose params are non-empty and do not include the implicit `_` keeps the
enclosing topic — pointy blocks, placeholder blocks (`{ $^x ~ $_ }`),
arity-2 blocks, and `$_`-referencing WhateverCode alike. Only a bare block
(or a plain `*.foo` WhateverCode, whose placeholder IS `_`) topicalizes the
element. The map fast path's arity>1 branch was also routed through
`bind_loop_topic` so it obeys the same rule. Every expectation was verified
against rakudo first (`for "o" { <a b>.map(-> $c { $c ~ $_ }) }` is
`("ao", "bo")`, etc.); pinned in `t/block-topic-explicit-params.t`.

This was the root cause of most of `http-cookiejar.rakutest`'s failures:
`Cro::HTTP::Client::CookieJar.add-from-response`'s uniqueness check
(`@!cookies.grep(-> $cs { checker($_, $cs) })`, where `$_` is the
`for $resp.cookies` topic) never matched, so every re-add duplicated cookies
("Use of Nil in string context" warnings from CookieJar line 106). With the
fix the file goes 22/30 → **30/30 fully green**.

Remaining related edge (not fixed here): rakudo resolves a stored block's
non-declared `$_` as a definition-site lexical capture (`my $b = -> $c { $_
}; given "outer" { $b("elem") }` is `Any` in rakudo — the definition-site
topic), while mutsu resolves it dynamically at call time (`"outer"`). The
iteration paths no longer hit this, and no known test depends on it; worth a
ticket if it ever surfaces.
