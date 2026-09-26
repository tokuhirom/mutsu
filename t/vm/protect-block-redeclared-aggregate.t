use v6;
use Test;

# Once a thread has run, a `start` publishes the spawning frame's `@`/`%`
# lexicals on the bare-name cross-thread lane. A later invocation's fresh
# `my @x` is a different container that merely shares the name; a
# `$lock.protect: { @x ... }` block in that invocation used to read the
# earlier invocation's `@x` from the lane (#8380: `Test::Scheduler.run-due`
# re-queued a cancelled `:every` event forever).

plan 4;

my $lock = Lock.new;

sub observe($spawn) {
    my @seen = 1;
    my @kept;
    for @seen {
        start { 1 } if $spawn;
        next unless $spawn;
        @kept.push($_);
    }
    my $inside = $lock.protect: { @kept.elems };
    (@kept.elems, $inside)
}

{
    my ($outside, $inside) = observe(True);
    is $inside, $outside, 'first call: protect block sees the frame\'s @kept';
}
{
    my ($outside, $inside) = observe(False);
    is $outside, 0, 'second call: a fresh, empty @kept';
    is $inside, 0, 'the protect block sees the fresh @kept, not the first call\'s';
}

sub hash-observe($fill) {
    my %h;
    start { 1 } if $fill;
    %h<a> = 1 if $fill;
    $lock.protect: { %h.elems }
}
hash-observe(True);
is hash-observe(False), 0, 'the same holds for a hash';

# vim: expandtab shiftwidth=4
