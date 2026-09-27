use Test;

# `self` inside an attribute initializer is the object under construction —
# the one `new`/`bless` returns — not a snapshot of it. A closure created by
# an initializer keeps that `self`, so it must see the identity and every
# attribute set after it ran (FINALIZER's role
# `has &!finalizer = FINALIZER.register: { self.finalize() }`).

plan 8;

class Later {
    has $.a = 1;
    has &.cb = { self.b };
    has $.b = 2;
}
my $later = Later.new;
is $later.cb.(), 2, 'a closure from an initializer sees an attribute initialized after it';
ok $later.cb.() =:= $later.b, '... the very same value';

class Same { has &.me = { self } }
my $same = Same.new;
ok $same.me.() === $same, 'the initializer\'s `self` is the constructed object';

class Blessed {
    has &.me = { self };
    has $.x;
    method new(:$x) { self.bless(:$x) }
}
my $blessed = Blessed.new(x => 5);
ok $blessed.me.() === $blessed, 'the same holds through a custom `new` calling `bless`';
is $blessed.me.().x, 5, '... and it sees the blessed attributes';

my @registered;
class Finalizing {
    has &!finalizer = do { @registered.push: { self.finalize }; -> { 'unregistered' } };
    method finalize { &!finalizer() }
}
my $f = Finalizing.new;
is @registered[0](), 'unregistered',
    'a block registered from an initializer can reach the attribute it initialized';

role Finalizable {
    has &!fin = do { @registered.push: { self.fin-it }; -> { 'role fin' } };
    method fin-it { &!fin() }
}
class UsesRole does Finalizable { }
UsesRole.new;
is @registered[1](), 'role fin', '... also for an attribute a role composes in';

class Built {
    has $.seen;
    has &.cb = { self.seen };
    submethod BUILD(:$!seen = 'from BUILD') { }
}
is Built.new.cb.(), 'from BUILD', 'an initializer deferred past BUILD sees what BUILD set';
