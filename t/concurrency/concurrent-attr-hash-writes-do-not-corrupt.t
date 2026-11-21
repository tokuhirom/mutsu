use Test;

# #11701: Method::Protected's test hammers one `has %!hash` from three threads
# under its lock. With the lock not in effect the process died with SIGSEGV in
# `drop_in_place::<GcBox<HashData>>`. The structure writes are since serialized
# on the ADR-0068 stripes.
#
# ADR-12814 (#18430): the guarantee is rakudo's. A program that synchronizes
# its shared hash is memory-safe and loses no update; one that races a hash
# from several threads with no synchronization has no defined result. rakudo
# 2026.11 dies on the unsynchronized version of every block below
# (`MoarVM oops: MVM_str_hash_fetch_nocheck called with a stale hashtable
# pointer`, 6 runs of 6), and mutsu panicked on it about one run in fifteen, so
# that version is no longer asserted here. Each block synchronizes every access
# with a `Lock`, as Raku asks, and checks both the structure and the exact
# result the serialization implies.

plan 9;

my @words = (('a'..'z').list, (('a'..'z') X~ ('a'..'z')).list).flat.list;   # 702 keys
my %legal = @words.map({ $_ => True });

sub hammer(&body) {
    await (^3).map: { start { for ^6000 { body() } } };
}

# `%!h{$k}++` in a method: the read-modify-write route (PostIncrementIndex).
{
    class Counter { has %!h; has $!lock = Lock.new;
                    method hit { $!lock.protect: { %!h{@words.pick}++ } }
                    method keys-seen { %!h.keys.sort.list }
                    method total { [+] %!h.values } }
    my $c = Counter.new;
    hammer({ $c.hit });
    my @k = $c.keys-seen;
    is @k.elems, @k.unique.elems, 'incr: every key is distinct';
    ok @k.all ~~ %legal, 'incr: no key outside the written set';
    is $c.total, 3 * 6000, 'incr: no update is lost';
}

# `%!h{$k}:delete` interleaved with inserts.
{
    class Churn { has %!h; has $!lock = Lock.new;
                  method go { $!lock.protect: { %!h{@words.pick} = 1; %!h{@words.pick}:delete } }
                  method keys-seen { %!h.keys.sort.list } }
    my $c = Churn.new;
    hammer({ $c.go });
    my @k = $c.keys-seen;
    is @k.elems, @k.unique.elems, 'delete: every key is distinct';
    ok @k.all ~~ %legal, 'delete: no key outside the written set';
}

# A leaf read (`.keys`, `.elems`) interleaved with a structural write.
{
    class Reader { has %!h; has $!lock = Lock.new;
                   method go { $!lock.protect: { %!h{@words.pick} = 1; %!h.keys.elems } } }
    my $c = Reader.new;
    my $max = 0;
    my $max-lock = Lock.new;
    hammer({ my $n = $c.go; $max-lock.protect: { $max max= $n } });
    ok $max <= 702, 'read: .keys.elems never exceeds the possible key count';
}

# The same read-modify-write on a file-scope hash a method reaches by name.
{
    my %shared;
    my $lock = Lock.new;
    class Global { method hit { $lock.protect: { %shared{@words.pick}++ } } }
    my $g = Global.new;
    hammer({ $g.hit });
    my @k = %shared.keys.sort.list;
    is @k.elems, @k.unique.elems, 'file-scope hash: every key is distinct';
    ok @k.all ~~ %legal, 'file-scope hash: no key outside the written set';
    is ([+] %shared.values), 3 * 6000, 'file-scope hash: no update is lost';
}
