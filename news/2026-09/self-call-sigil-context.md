# `@.meth: args` and `@.meth(args)` call the method on self in list context

[#9323](https://github.com/tokuhirom/mutsu/issues/9323), split out of the
[#7988](https://github.com/tokuhirom/mutsu/issues/7988) parse-gap cluster.
PDF::Font::Loader's `Enc/CMap.rakumod` has `@.protect: { @bytes.map: {...} }`,
which mutsu rejected with `===SORRY!=== Confused`, so the module could not
load (and neither could the other PDF-* distributions that depend on it).

rakudo's `variable` token lets a `.`-twigil variable carry its own argument
list, either a `(...)` postcircumfix or a `:` followed by whitespace and an
arglist, and then applies the sigil's contextualizer to the call's result.
So `@.protect: { 42 }` is `@(self.protect({ 42 }))` and prints `(42)`, and
`%.meth: 1` is `%(self.meth(1))`.

mutsu already handled the `$` spelling by leaving `$.meth(...)` /
`$.meth: ...` to the postfix parser as a call on `self`. The `@` and `%`
spellings were only bare accessors. The colon form did not parse at all, and
`@.p(1)` parsed as *calling the result* of the zero-argument accessor, so it
died with "Too few positionals passed".

The new `var/self_call.rs` parses exactly one argument list after `@.name` or
`%.name`. It builds `self.name(args)` wrapped in `.list` or `.hash`, and
leaves any later postfix to the ordinary postfix loop. An empty colon arglist
(`@.meth: ;`) is a zero-argument call. The bare `@.attr` / `%.attr` accessor
form is unchanged.

Pinned by `t/oo/method/method-self-sigil-context.t`.
