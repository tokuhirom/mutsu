# A resuming CATCH or CONTROL handler writes its own block's lexicals

A `CONTROL` handler that resumes a `warn` from a sub runs at the `warn`, but
its lexicals belong to the block that declared it. When the warning sub had a
lexical of the same name, the handler's write went to the sub's variable:

```raku
sub w { my $m = 'callee'; warn 'W'; $m }
{
    my $m = '';
    CONTROL { when CX::Warn { $m = .message; .resume } }
    say w();   # was W, now callee
    say $m;    # now W
}
```

The handler now writes the installing block's slots directly, for `CATCH`
and `CONTROL` alike, and leaves the env of the frame that threw alone. Its
writes no longer depend on each frame's return merge to carry them back up,
which removes one more dependency on that merge (ADR-12529 §3, slice 22).
