# A trailing `BEGIN` is the block's value

`BEGIN` runs at compile time but is still an ordinary value-producing statement:
in value-final position it is the block's value. mutsu handled the *expression*
form (`my $a = BEGIN 42`) and dropped the *statement* form, so a block ending in
a `BEGIN` yielded `Nil`:

```raku
sub tail() { BEGIN 'hello' }
tail();                                   # raku: "hello"    mutsu was: Nil

sub fallback($h) { with $h { … } else { BEGIN 'default' } }
fallback(Nil);                            # raku: "default"  mutsu was: Nil
```

`Cro::HTTP::Body::MultiPartFormData::Part` expresses its content-type default
exactly that way:

```raku
method content-type() {
    with @!headers.first(*.name.lc eq 'content-type') {
        Cro::MediaType.parse(.value)
    }
    else {
        BEGIN Cro::MediaType.new(type => 'text', subtype-name => 'plain')
    }
}
```

so every multipart part without a `Content-Type` header had **no content type at
all** — and `Part.body`, which dispatches on it to pick a body parser, failed
with it.

## Fix

Three value-final compilation paths gained a `PhaserKind::Begin` arm, all
delegating to a new `compile_check_phaser_value`:

- `compile_stmts_value` — an `if`/`else` branch's value;
- `compile_sub_body`'s tail-statement match — a routine's implicit return;
- `compile_when_tail_stmt` — a `given`/`when`/`default` block's value, which is
  the path Cro reaches, since `with … else …` lowers to an `if` whose branches
  are `given`s.

The helper compiles the body exactly as the rvalue form does, through
`OpCode::BeginOnceExpr`. That matters as much as the value itself: without the
per-site memo a `BEGIN` in a routine tail would re-evaluate on every call, which
is precisely what `BEGIN` promises not to do. (Like the rvalue form, mutsu
evaluates it at first use rather than at true parse time — a documented
deviation, see `Compiler::compile_phaser_expr`.)

## Result

In Cro's `t/http-request-parser.rakutest` the passing assertion count goes from
**323 to 334** and failures from 18 to 7: all twelve multipart per-part
content-type assertions now pass, along with `Part.body`.

Pinned by `t/begin-in-value-position.t` (10 assertions, byte-identical output
under real `raku`).

One neighbouring shape is still rejected — `constant X = BEGIN …` dies with
"Cannot assign to a readonly variable" — recorded in
`todo/tickets/constant-declared-from-a-begin-is-rejected.md`.
