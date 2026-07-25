# Seven general fixes surfaced by the HTTP::UserAgent test suite

Driving the upstream HTTP::UserAgent suite (27 files, `raku-community-modules/HTTP-UserAgent`
at `1d6a31a`) against mutsu took it from **17 to 19 fully-passing files**, and most of the
remaining files went from many failures to one. Every fix below is a general interpreter bug
that the module merely happened to exercise first; none of them is HTTP-specific.

## A scalar interpolated into a regex could not contain whitespace

`escape_regex_scalar_literal` escaped every whitespace character with a backslash. But `\ ` in
regex source is the *unspace* form, which raku rejects outright ("No unspace allowed in
regex"); mutsu's own parser rejects it too. The escaped whitespace was therefore silently
dropped at the end of a pattern, so `"a\r\nb".split(/$CRLF/)` never split — the single most
load-bearing line of `HTTP::Message.parse`. Whitespace now becomes a `\x[HH]` codepoint escape
(same fix in `regex_escape_literal`, used for `:sym<…>` values). Pin:
`t/regex-interp-whitespace-literal.t`.

## An `is rw` multi candidate was unreachable through a `proto method`

An `is rw` parameter only binds a writable argument, so multi dispatch uses it to narrow
candidates — mutsu checks the call site's argument sources for that. Running a `proto method
f(|) {*}` body cleared those sources, and `proto_rw_redispatch_args` only rebuilds them when
the *proto's own* signature declares an rw parameter (which `(|)` does not). The rw candidate
then matched nothing. `ProtoMethodCtx` now carries the call site's argument sources across the
proto body into the `{*}` redispatch. Separately, `is rw` now counts as a narrowing constraint
in the method-side specificity tie-break (it already did on the sub side), so
`(Int $b is rw)` beats `(Int $b)` for a variable argument instead of being ambiguous. Pin:
`t/multi-method-rw-param-dispatch.t`.

## A `:D` smiley on an unsupplied optional vetoed its own candidate

`multi method new(Int:D $code = 200, *%fields)` did not match a bare `.new()`. An unsupplied
optional binds its default (or, with no default, the bare type object), and mutsu stands in the
type object for the dispatch-time checks — against which `:D` always fails. Raku checks
definedness at *bind* time, against the default value; at dispatch time even `Int:D $x?`
matches. The definedness smiley is now dropped when nothing was passed; the nominal type still
discriminates candidates. Pin: `t/multi-optional-default-smiley-dispatch.t`.

## A class from a `use`d module was not a valid parameter type

`use URI; sub f(URI $u) { }` died with "Invalid typename 'URI' in parameter declaration". The
compile-time parameter-type check runs before the mainline — and therefore before `use` has
loaded anything — so nothing the module declares was visible; only `::`-qualified names escaped
the check. The pre-pass now harvests type names straight from each `use`d module's source text
(a keyword scan, not a full parse, so it does not duplicate the load the mainline is about to
do), honouring `use lib '...'` paths as well as `-I`/`MUTSULIB`/bundled ones. Over-collecting
is harmless — the set only widens what is accepted, and a genuine typo appears in no module's
source, so `sub yoink(Junctoin $barf)` is still rejected. Pin:
`t/param-type-from-used-module.t`.

## `POST($x)` was parsed as the POST phaser

Raku decides between a phaser and a call on the space: `BEGIN (1+2)` is the phaser over a
parenthesised statement, `BEGIN(1+2)` is a call to a routine named `BEGIN`. mutsu took the
phaser branch either way, so `HTTP::Request::Common`'s exported `POST`/`PUT` subs — which
recurse as `POST($uri, …)` — silently returned `Nil` from the swallowed statement. Both the
statement-level (`phaser_stmt`) and expression-level (`term_literals`) phaser parsers now
require that the keyword is not immediately followed by `(`. Pin:
`t/phaser-keyword-call-with-parens.t`.

## `$x ~= "a"` warned on an undefined `$x`

`OP=` on an undefined lvalue seeds the container with `infix:<OP>`'s zero-arg identity — mutsu
already did this for `*=`/`**=` (identity 1) and `%=` (which has none, so it throws), but not
for `~=`, whose identity is `''`. Without it every `$.content ~= $chunk` on a fresh message
emitted a spurious "Use of uninitialized value of type Any in string context". The mutating-
method compound-assign path (`$o.v ~= …`) also built its `Binary` directly instead of going
through the shared lowering, and now shares it. A plain `$u ~ "a"` still warns. Pin:
`t/concat-assign-undefined-identity.t`.

## `utf8.Str` threw instead of decoding

`utf8` is the one Blob flavour whose `.Str` works in rakudo: it decodes as UTF-8, while `Buf`,
`Blob` and `Blob[uint8]` all throw `X::Buf::AsStr`. mutsu threw for all of them, so
`"bumble".encode eq "bumble"` was False and `is $req.content.encode, "bumble"` failed. `.Str`/
`.Stringy`, the string-comparison operand coercion, and `Test`'s `is` now all decode a `utf8`
(falling back to the byte-wise comparison when the bytes are not valid UTF-8). Pin:
`t/utf8-str-decodes.t`.
