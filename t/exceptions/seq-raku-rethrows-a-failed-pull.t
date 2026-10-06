use v6;
use Test;

# `.raku` of a deferred Seq pulls its source to render the elements, and used
# to swallow whatever that pull died with: a Seq whose `.map` block dies
# rendered as the `Seq.new()` placeholder instead of raising the block's
# exception. Only the `X::Seq::Consumed` of an already spent Seq is the
# placeholder (rakudo: `.raku` on a consumed Seq does not throw, unlike
# `.gist`/`.Str`).
#
# Not pinned here: what the Seq does on the READS THAT FOLLOW a failed one.
# Rakudo keeps the partially reified list and resumes the iterator after the
# failing element; mutsu leaves the Seq consumed (see ADR-0034's amendment of
# 2026-10-06 and the follow-up issue).

plan 12;

sub died-with(&code) { try { code(); CATCH { default { return .message } } }; 'no error' }

# The issue's repro: the Seq is built inside a `try`, nothing has run yet.
my $r = try { (1, 2).map({ die 'boom' }) };
is died-with({ $r.raku }), 'boom', '.raku raises the exception the pull died with';

my $m = (1, 2).map({ die 'boom' });
is died-with({ $m.raku }), 'boom', '... for a Seq that was not built in a try block';
my $gist-first = (1, 2).map({ die 'boom' });
is died-with({ $gist-first.gist }), 'boom', '.gist of a fresh one agrees (the same exception, as before)';

# The exception is the thrown object.
my $t = (1, 2).map({ X::AdHoc.new(payload => 'typed').throw });
my $err = do { try $t.raku; $! };
isa-ok $err, X::AdHoc, '.raku dies with the exception object that was thrown';
is $err.payload, 'typed', '... carrying its payload';

# Other deferred sources.
my $g = (1, 2).grep({ die 'grep-boom' });
is died-with({ $g.raku }), 'grep-boom', 'a dying .grep';

class DyingIterator does Iterator { method pull-one { die 'iter-boom' } }
my $i = Seq.new(DyingIterator.new);
is died-with({ $i.raku }), 'iter-boom', 'a Seq over a dying Iterator';

# What must not change.
my $ok := (1, 2, 3).map({ $_ * 2 });
is $ok.raku, '(2, 4, 6).Seq', 'a healthy deferred Seq renders its elements';
is $ok.raku, '(2, 4, 6).Seq', '... and renders them again';

my $consumed := (1, 2, 3).map({ $_ });
$consumed.List;
is $consumed.raku, 'Seq.new()', '.raku of an already consumed Seq is still the placeholder';
like died-with({ $consumed.gist }), /'already in use/consumed'/, '... while .gist still reports it consumed';

my $stolen := (1, 2).map({ die 'stolen-boom' });
is died-with({ $stolen.sort }), 'stolen-boom', 'a consuming read dies with the block\'s exception';
