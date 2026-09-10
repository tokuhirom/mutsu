use v6;
use Test;

# Regression: a `:=` bind of a callee-local container (`$!attr := @p`) inside a
# nested method/protect block used to propagate its shared cell into EVERY saved
# call frame's `saved_locals` by the CALLEE's slot index, clobbering an unrelated
# caller lexical that happened to share that slot index. Concretely, mainline's
# `$config` (a Hash at slot 1) got overwritten by the callee's empty `@p` (also
# slot 1 in the callee), so `$config.^name`/`.WHICH` reported `Array` while the
# value was still a genuine Hash. See tmp/lexical-corruption-repro.raku.

plan 6;

role Plug {
    has $!plugins;
    has $!lock = Lock.new;
    method plugins() {
        $!lock.protect: {
            return $!plugins if $!plugins.so;   # guard that does NOT fire
            my @p;
            return $!plugins := @p;             # `:=` bind of a callee-local
        }
    }
}
class Repo does Plug {
    submethod TWEAK { self.plugins; }
}

my %h = a => 1, b => 2;
my $config = %h;
my $r = Repo.new;

is $config.^name, 'Hash', 'caller lexical keeps its type name after nested :=';
is $config.WHICH.Str.substr(0, 5), 'Hash|', '.WHICH still reports a Hash';
ok $config ~~ Hash, 'value still smartmatches Hash';
nok $config ~~ Array, 'value does not smartmatch Array';
is $config<a>, 1, 'associative indexing still works';
is $config.keys.sort.join(','), 'a,b', 'keys are intact';
