use Test;

# #18266: the type registry is shared by every thread (ADR-17216), and a mixin
# type `C+{R}` is composed in several registry writes. Threads that asked for
# the same mixin type at the same moment used to find it registered but not
# yet composed, and then failed with "No such method 'mark'" on it. Twelve
# threads race to mix three roles into one shared pun; every one must get a
# fully composed type, run after run.

plan 2;

role Conc::Maker[::T = Any] { has $.value; method kind { 'maker' } }
role M0 { method mark { 'm0' } }
role M1 { method mark { 'm1' } }
role M2 { method mark { 'm2' } }

my @roles = M0, M1, M2;
my $pun = Conc::Maker.^parameterize(Any).^pun;
my $go = Promise.new;
my @out;
my $out-lock = Lock.new;
my @threads = (^12).map: -> $i {
    Thread.start({
        await $go;
        my $r = try {
            my \T = $pun but @roles[$i % 3];
            (T.new(value => $i).kind, T.new(value => $i).mark).join(',')
        } // "ERR: $!";
        $out-lock.protect: { @out.push: $r };
    })
};
$go.keep;
.finish for @threads;

is @out.elems, 12, 'every thread finished';
is-deeply @out.sort.unique.List, <maker,m0 maker,m1 maker,m2>,
    'every thread got a fully composed mixin type';
