use Test;

# The callable `nextcallee` returns in a method wrapper passes every kind of
# argument through to the candidate: positionals, named arguments, a slurpy,
# a writable container, and an `is raw` return value. It also keeps working
# from a closure and through `.()`. A call on it runs the chain directly,
# without the continuation template's own call (#12875). These pin that this
# shortcut forwards exactly what the template forwarded.

plan 9;

class C {
    has $.seen;
    method m($a, $b = 'B', :$n = 'N', *%rest) {
        "$a|$b|$n|" ~ %rest.sort.map({ .key ~ '=' ~ .value }).join(',')
    }
    method bump($x is rw) { $x++ }
    method slot() is raw { $!seen }
}

my &fwd = my method (|c) { my &original = nextcallee; original(self, |c) };
C.^lookup('m').wrap(&fwd.clone);
C.^lookup('bump').wrap(&fwd.clone);
C.^lookup('slot').wrap(my method (|c) is raw {
    my &original = nextcallee;
    original(self, |c)
});

my $o = C.new;
is $o.m(1), '1|B|N|', 'a positional';
is $o.m(1, 2), '1|2|N|', 'two positionals';
is $o.m(1, :n<x>), '1|B|x|', 'a named argument';
is $o.m(1, :n<x>, :p(1), :q(2)), '1|B|x|p=1,q=2', 'named arguments into a slurpy';

my $v = 5;
$o.bump($v);
is $v, 6, 'an is rw parameter writes through the continuation';

$o.slot = 'set';
is $o.seen, 'set', 'an is raw return stays a container through the continuation';

class D {
    method m($x) { "D:$x" }
}
D.^lookup('m').wrap(my method (|c) {
    my &original = nextcallee;
    my $r;
    { $r = original(self, |c) }();
    $r
});
is D.new.m(7), 'D:7', 'called from a closure';

class E {
    method m($x) { "E:$x" }
}
E.^lookup('m').wrap(my method (|c) {
    my &original = nextcallee;
    &original.(self, |c)
});
is E.new.m(8), 'E:8', 'called with .()';

my &cont = do {
    class F { method m($x) { "F:$x" } }
    my &k;
    F.^lookup('m').wrap(my method (|c) { &k = nextcallee; 'wrapped' });
    F.new.m(1);
    &k
};
is cont(F.new, 9), 'F:9', 'a continuation called after its wrapper returned';
