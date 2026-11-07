use v6;
use Test;

# A type cached in a shared Map and built by whichever thread asks first must
# be usable from every thread. The pun class of a parametric role
# (`Conc::Maker[Any]`) is composed lazily on the thread that first needs it; the
# others used to fail with "Could not find symbol '&Maker[Any]'" because their
# registry snapshot had never heard of it (Monad t/14_concurrent_types).

plan 3;

role Conc::Maker[::T = Any] {
    has $.value;
    method kind { 'maker' }
}
role Marker { method mark { 'marked' } }

my $lock = Lock.new;
my $cache = Map.new;
sub cached-pun() {
    return $cache<k> if $cache<k>:exists;
    $lock.protect: {
        return $cache<k> if $cache<k>:exists;
        my $pun = Conc::Maker.^parameterize(Any).^pun but Marker;
        my %next = $cache;
        %next<k> = $pun;
        $cache = %next.Map;
        $pun;
    }
}

my $go = Promise.new;
my @out;
my $out-lock = Lock.new;
my @threads = (^6).map: -> $i {
    Thread.start({
        await $go;
        my $r = try {
            my \T = cached-pun();
            (T.new(value => $i).kind, T.new(value => $i).mark).join(',')
        } // "ERR: $!";
        $out-lock.protect: { @out.push($r) };
    })
};
$go.keep;
.finish for @threads;

is @out.elems, 6, 'every thread finished';
is-deeply @out.unique.List, ('maker,marked',), 'every thread built and mixed into the shared pun';

my \Pun = cached-pun();
is Pun.new(value => 1).kind, 'maker', 'and the main thread still can';
