use Test;

# A private method call (`self!name(...)`) is replayed from a per-site
# method-call plan after its first call with those argument types
# (ADR-15737 D1). The plan must answer exactly as the private dispatch arm
# would on every call: for a private method a role brings in, for one called
# from a subclass's own method, through `trusts`, after the class gains a
# method at run time, after the private method is wrapped, and for a caller
# that may not dispatch it at all.
plan 8;

class Counter {
    has $!n = 0;
    method !bump(Str:D $t, Bool:D $v --> Nil) { $!n += $t.chars + ($v ?? 10 !! 0) }
    method run(Str:D $t) { self!bump($t, False) for ^5; self!bump($t, True); $!n }
}
is Counter.new.run('ab'), 22, 'a planned private call updates attributes on every call';

role Greets {
    method !greet(Str $who) { "hi $who" }
    method greet-all(*@who) { @who.map({ self!greet($_) }).join(',') }
}
class Host does Greets { }
is Host.new.greet-all(<a b c a>), 'hi a,hi b,hi c,hi a', 'a private method from a role';

class Parent {
    method !secret(Int $x) { "parent $x" }
    method reveal(Int $x) { self!secret($x) }
}
class Child is Parent {
    method !own-secret(Int $x) { "child $x" }
    method own(Int $x) { self!own-secret($x) }
}
my $c = Child.new;
is (^4).map({ $c.own($_) ~ '/' ~ $c.reveal($_) ~ '/' ~ Parent.new.reveal($_) }).join(' '),
    'child 0/parent 0/parent 0 child 1/parent 1/parent 1 child 2/parent 2/parent 2 child 3/parent 3/parent 3',
    'a subclass instance reaches a parent\'s private method next to its own';

class Vault { ... }
class Auditor {
    method peek(Vault $v) { $v!Vault::balance() }
}
class Vault {
    trusts Auditor;
    has $.amount = 7;
    method !balance() { $!amount * 2 }
    method self-check() { self!balance() }
}
my $v = Vault.new;
my $a = Auditor.new;
is (^3).map({ $a.peek($v) ~ ':' ~ $v.self-check }).join(' '), '14:14 14:14 14:14',
    'a trusted class calls the private method next to planned own calls';

class Late {
    method !tag(Int $x) { "tag $x" }
    method use(Int $x) { self!tag($x) }
}
my $l = Late.new;
my @before = (^3).map({ $l.use($_) });
Late.^add_method('extra', method () { 'extra' });
is (@before, (^3).map({ $l.use($_) }), $l.extra).flat.join(','),
    'tag 0,tag 1,tag 2,tag 0,tag 1,tag 2,extra',
    'a method added at run time leaves the private call intact';

class Wrapped {
    method !w(Int $x) { $x * 2 }
    method go(Int $x) { self!w($x) }
}
my $w = Wrapped.new;
my @plain = (^3).map({ $w.go($_) });
Wrapped.^find_private_method('w').wrap(-> $self, $x { callsame() + 100 });
is (@plain, (^3).map({ $w.go($_) })).flat.join(','), '0,2,4,100,102,104',
    'wrapping a planned private method takes effect on the next call';

class Shy {
    method !hidden(Int $x) { "hidden $x" }
    method ok(Int $x) { self!hidden($x) }
}
my $s = Shy.new;
is (^3).map({ $s.ok($_) }).join(','), 'hidden 0,hidden 1,hidden 2',
    'own calls are planned';
throws-like { EVAL 'class Outsider { method poke($s) { $s!Shy::hidden(1) } }; Outsider.new.poke($s)' },
    X::Method::Private::Permission, 'an untrusting class still refuses an outside caller';
