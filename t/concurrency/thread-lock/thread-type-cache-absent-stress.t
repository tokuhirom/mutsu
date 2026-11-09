use Test;
# The shape of Monad's type-hook cache: an immutable Map published by replacing
# a scalar under a Lock, lock-free readers that tell "absent" from a cached type
# object with sigilless `=:=`, and `pun but Role` / parameterization types built
# on whichever thread misses. Four threads must never see a stale `Any` as a
# cached type, nor a mixin type that lost its role. Identical under rakudo.
plan 3;

role R[::T = Any] { has $.v; method mk(\v) { self.new(:v(v)) } }
role M1 { }
role M2 { }

my $lock = Lock.new;
my $cache = Map.new;
sub absent(Mu \candidate --> Bool) { candidate =:= Nil || candidate =:= Any }
sub publish(Str \key, &build --> Mu) {
    $lock.protect({
        my \raced = $cache{key};
        if absent(raced) {
            my \built = build();
            my %next = $cache;
            %next{key} = built;
            $cache = %next.Map;
            built;
        }
        else { raced }
    });
}
sub get(Str \key, &build --> Mu) {
    my \hit = $cache{key};
    absent(hit) ?? publish(key, &build) !! hit;
}
sub parameterized($k) { get("p$k", { R.^parameterize(Int) }) }

my $go = Promise.new;
my @workers = (^4).map: -> $n {
    my @errs;
    Thread.start({
        await $go;
        for ^250 -> $i {
            my $k = $i % 15;
            my \P = parameterized($k);
            my \U = get("u$k", { parameterized($k).^pun but M2 });
            my \V = get("v$k", { R.^pun but M1 });
            my $o = U.mk($i);
            @errs.push("U {U.^name} gave {$o.^name}") unless $o ~~ M2 && $o.v == $i;
            my $p = V.mk($i);
            @errs.push("V {V.^name} gave {$p.^name}") unless $p ~~ M1 && $p.v == $i;
            @errs.push("P {P.^name}") unless P.^name eq 'R[Int]';
            last if @errs;
        }
        CATCH { default { @errs.push("EXC: {.message.lines.head}") } }
    }) => @errs
};
$go.keep;
.key.finish for @workers;

my @all = @workers.map(*.value).flat;
is @all.elems, 0, 'no worker saw a wrong type' or diag @all.head(3).join("\n");
is $cache.elems, 45, 'the cache holds every key once';
ok absent($cache<missing>), 'a missing key is absent';
