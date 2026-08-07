# A `whenever` block's parameter type constraint is not enforced

A `whenever` signature parameter accepts any value, so a type error surfaces
later as a missing method on the wrong type instead of at the binding.

```raku
my $s = Supplier.new;
my $o = supply { whenever $s -> Int $x { emit $x } };
$o.tap(-> $v { say "out $v" });
$s.emit("str");
```

raku dies with `Type check failed in binding to parameter '$x'; expected Int but
got Str ("str")`. mutsu prints `out str`.

The same constraint on a plain `.tap` block *is* enforced
(`$s.Supply.tap(-> Int $x { … })` dies correctly), so the gap is specific to the
callback `run_whenever_with_value` builds: it is created with
`Value::make_sub_owning(…, param.iter().cloned().collect(), …)` — a bare name
list with the parsed type constraint dropped on the floor
(`src/runtime/subtest.rs`).

## Why it is worth fixing

It turns type errors into confusing downstream failures. Diagnosing the Cro
chunked-body leak (`news/2026-08/nested-sub-emit-routes-to-its-own-supply.md`)
cost extra time because `Cro::HTTP::ResponseParser`'s
`whenever $in -> Cro::TCP::Message $packet { … $packet.data … }` silently
accepted a leaked `Buf` and reported `No such method 'data' for invocant of type
'Buf'` instead of a binding failure naming the parameter.

## Where to look

`Interpreter::run_whenever_with_value` (`src/runtime/subtest.rs`) takes
`param: &Option<String>` — the name only. The parser would need to carry the
`whenever` parameter's declared type through to it, and the callback would need
to be built with the same signature machinery an ordinary pointy block uses.
