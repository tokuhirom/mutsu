use Test;

plan 3;

# A statement whose text ends with a `}` at end of line is self-terminating,
# including when the block belongs to a lowered block-taking construct passed
# as a colon argument: `$obj.method: supply { ... }` followed by a newline
# must NOT swallow the next line's `given`/`if` as a statement modifier.
# (Cro::Core t/message-with-body.rakutest silently skipped its `given await
# ... -> $blob { ok ... }` tests this way.)

my class Holder {
    has $.stream;
    method set-stream($!stream) { }
}

my $h = Holder.new;
$h.set-stream: supply {
    emit 42;
}
given 7 -> $v {
    is $v, 7, 'given after a colon-supply-arg statement runs as its own statement';
}

my $ran = False;
$h.set-stream: supply {
    emit 1;
}
if True {
    $ran = True;
}
ok $ran, 'if after a colon-supply-arg statement runs as its own statement';

ok $h.stream ~~ Supply, 'the colon argument itself still bound correctly';
