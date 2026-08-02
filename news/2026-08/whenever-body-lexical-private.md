# A `whenever` body's own `my` lexicals stay private to the block

`news/2026-08/supply-block-lexical-privacy.md` made a `supply { my $x … }`
block's lexicals private. The `whenever` bodies *inside* such a block were still
leaking their own: a `my` declared in the body was written back into whatever
scope invoked it — which, for a supply driven from a `start` block, is that
thread's scope. The leak then rebound a caller lexical of the same name between
two emits:

```raku
sub mk($in) {
    supply {
        whenever $in -> $packet {
            my $data = "INNER-" ~ $packet;
            emit $data;
        }
    }
}

my $data = "OUTER";
...
start { $s.emit($data); $s.emit($data); $s.done }
# raku:  INNER-OUTER|INNER-OUTER
# mutsu: INNER-OUTER|INNER-INNER-OUTER
```

The second emit read the *body's* `$data`, not the script's.

The exit merge in `call_sub_value` already skipped names the body declared with
`my` — but it read them off `SubData::compiled_code`, and a `whenever` callback
has none: `run_whenever_with_value` builds it from AST, and `call_sub_value`
compiles that AST on the fly through `eval_block_value`. The compile-time
`my_declared_sym` existed, just on a chunk the merge could not see.

`eval_block_value_inner` now publishes that set (its own `my` declarations minus
the names it also uses as free variables) in `Interpreter::last_block_my_declared`,
written just before it returns, so after a call the value belongs to the
outermost block that completed — nested blocks publish and are overwritten first.
`call_sub_value` takes it immediately after the body runs and folds it into the
existing `is_body_private` test. The rule is unchanged; it just now reaches every
body, not only those carrying a `CompiledCode`.

## Effect

The eight remaining failures of upstream `t/http2-frame-parser.rakutest` were all
`test-dying` cases, and none of them was about exception propagation at all.
`Cro::HTTP2::FrameParser`'s `whenever` body opens with

```raku
whenever $in -> Cro::TCP::Message $packet {
    my $data = $buffer ~ $packet.data;
    ...
    $data .= subbuf(24);          # consume the HTTP/2 preface
```

while the test emits its frames from a `start` block holding its own
`my $data`. After the preface packet the body's `$data` is empty, that empty Buf
landed on the test's `$data`, and the second emit shipped a zero-byte message —
so the parser never reached the payload that was supposed to die, and the tap's
`quit` handler never fired. With the leak closed, the parser dies with
`X::Cro::HTTP2::Error` as it should.

Pin: `t/whenever-body-lexical-private.t` (also passes under `raku`).
