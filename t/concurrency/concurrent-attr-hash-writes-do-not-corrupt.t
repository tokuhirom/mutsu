use Test;

# #11701: Method::Protected's test hammers one `has %!hash` from three threads.
# With its lock not in effect the process died with SIGSEGV in
# `drop_in_place::<GcBox<HashData>>` (3 of 8 runs of the bare version below on a
# release build), and the runs that finished reported `.elems` of 697..803 for
# 702 possible keys -- a torn map. Rakudo promises no atomicity here, but it does
# not corrupt its heap either (ADR-0068; docs/security.md "Memory safety").
#
# Each block hammers one container for a fixed number of iterations from three
# threads and then asserts the one thing that must hold whatever the
# interleaving: the structure is intact (every key is distinct and present in
# the set of keys that could have been written). The old build died before the
# assertion or failed it.

plan 8;

my @words = (('a'..'z').list, (('a'..'z') X~ ('a'..'z')).list).flat.list;   # 702 keys
my %legal = @words.map({ $_ => True });

sub hammer(&body) {
    await (^3).map: { start { for ^6000 { body() } } };
}

# `%!h{$k}++` in a method: the read-modify-write route (PostIncrementIndex).
{
    class Counter { has %!h; method hit { %!h{@words.pick}++ }
                    method keys-seen { %!h.keys.sort.list } }
    my $c = Counter.new;
    hammer({ $c.hit });
    my @k = $c.keys-seen;
    is @k.elems, @k.unique.elems, 'incr: every key is distinct';
    ok @k.all ~~ %legal, 'incr: no key outside the written set';
}

# `%!h{$k}:delete` interleaved with inserts.
{
    class Churn { has %!h; method go { %!h{@words.pick} = 1; %!h{@words.pick}:delete }
                  method keys-seen { %!h.keys.sort.list } }
    my $c = Churn.new;
    hammer({ $c.go });
    my @k = $c.keys-seen;
    is @k.elems, @k.unique.elems, 'delete: every key is distinct';
    ok @k.all ~~ %legal, 'delete: no key outside the written set';
}

# A leaf read (`.keys`, `.elems`) racing a structural write.
{
    class Reader { has %!h; method go { %!h{@words.pick} = 1; %!h.keys.elems } }
    my $c = Reader.new;
    my $max = 0;
    hammer({ $max max= $c.go });
    ok $max <= 702, 'read: .keys.elems never exceeds the possible key count';
}

# The same read-modify-write on a file-scope hash a method reaches by name.
{
    my %shared;
    class Global { method hit { %shared{@words.pick}++ } }
    my $g = Global.new;
    hammer({ $g.hit });
    my @k = %shared.keys.sort.list;
    is @k.elems, @k.unique.elems, 'file-scope hash: every key is distinct';
    ok @k.all ~~ %legal, 'file-scope hash: no key outside the written set';
}

# The lock that motivated all this still excludes: with every access inside
# `Lock.protect` no update may be lost.
{
    class Locked { has %!h; has $.lock = Lock.new;
                   method hit { $!lock.protect: { %!h{@words.pick}++ } }
                   method total { [+] %!h.values } }
    my $c = Locked.new;
    hammer({ $c.hit });
    is $c.total, 3 * 6000, 'a Lock.protect-ed increment loses no update';
}
