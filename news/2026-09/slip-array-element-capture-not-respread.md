# A Capture inside a slipped container is one argument, not an argument list

`|EXPR` spreads the container it is applied to, one level. mutsu spread it
twice: `append_slip_item` re-examined every item the Slip carried and, if the
item was itself a `Capture`, replayed its positional and named lanes into the
argument list as well. So a Capture that merely *sat in* a slipped array was
dissolved:

```raku
sub inner(Str $n, *@c, *%a) { say @c.raku; say %a.raku }
sub outer(Str $n, *@contents, *%attribs) { inner($n, |@contents, |%attribs) }
outer('test', :type<embedded>, \('hello', :lang<en>, "world"));

# raku : [\("hello", "world", :lang("en"))] / {:type("embedded")}
# mutsu: ["hello", "world"]                 / {:lang("en"), :type("embedded")}
```

`exec_make_slip_op` already replays a genuine `|$capture`'s lanes into the
Slip's items when the Slip is built, so by the time `append_slip_item` runs
every remaining `Capture` item can only have come from a *container element* —
where `|` was applied to the container, not to the Capture. The re-spread arm
is gone; such an item is now pushed as one argument.

Found while re-measuring the `XML` battery candidate
(`todo/tickets/bundle-xml-battery.md`). `XML`'s `make-xml` is built on exactly
this relay:

```raku
sub make-xml (Str $name, *@contents, *%attribs) {
    XML::Element.craft($name, |@contents, |%attribs);
}
```

and `craft-new` branches on `$what ~~ Capture` to recurse into a nested
element. With the Capture dissolved, that arm never fired, so
`make-xml('test', :type<embedded>, \('hello', :lang<en>, "world"))` produced
`<test type="embedded" lang="fr">hello world …</test>` — the children's text
and attributes hoisted into the parent — instead of nested `<hello>` /
`<aurevoir>` elements. The suite moves from **13/15 to 14/15** files
(`raku`: 15/15); the one remaining failure, `t/namespaces.rakutest`, is an
unrelated defect in stringifying a list of objects that define `Str`.

Pin: `t/slip-array-element-capture.t`, which also covers the shapes that must
not change (`|$capture`, `|@array`, `|%hash`, `|(list)`, and a `Pair` element of
a slipped array staying positional).
