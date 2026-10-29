use Test;

# `@!x = ...` / `%!x = ...` reach the VM by name; the store site caches where
# the attribute lives. The cache must follow the instance's class, copy on
# assignment, and keep honouring typed and role-owned attributes.

plan 14;

class Base {
    has %!kids;
    has @!list;
    method set(%h, @l) { %!kids = %h; @!list = @l; self }
    method kids() { %!kids }
    method list() { @!list }
}
class Wide is Base { has $.extra = 1; has %!more; }

my %src = a => 1, b => 2;
my @lsrc = 1, 2, 3;

my $b = Base.new.set(%src, @lsrc);
my $w = Wide.new.set(%src, @lsrc);
# the same method body runs on two layouts
is $b.kids<a>, 1, 'base instance stores into its own attribute';
is $w.kids<b>, 2, 'subclass instance (another layout) stores into its own';
is-deeply $w.list, [1, 2, 3], 'array attribute on the subclass';

%src<a> = 99;
@lsrc.push(4);
is $b.kids<a>, 1, 'the attribute holds a copy of the source hash';
is-deeply $b.list, [1, 2, 3], 'the attribute holds a copy of the source array';

$b.set({ z => 26 }, [9]);
is-deeply $b.kids.keys.sort.list, ('z',), 'a second store replaces the contents';
is $w.kids<a>, 1, 'a store on one instance does not reach another';

# a store from a method of an unrelated class on the same call site
class Other {
    has %!kids;
    method set(%h) { %!kids = %h; self }
    method kids() { %!kids }
}
my @sets = (Base.new, Other.new, Wide.new);
for @sets -> $o { $o.set({ k => $o.^name }, []) if $o ~~ Base; $o.set({ k => $o.^name }) if $o ~~ Other }
is-deeply @sets.map(*.kids<k>).list, ('Base', 'Other', 'Wide'), 'one site, three classes';

# typed attributes keep their element type through the cached store
class Typed {
    has Int %!n;
    method set(%h) { %!n = %h; self }
    method n() { %!n }
}
my $t = Typed.new.set({ a => 1 });
is $t.n<a>, 1, 'typed hash attribute stores';
dies-ok { Typed.new.set({ a => 'x' }) }, 'typed hash attribute still rejects a bad value';
is Typed.new.set({ b => 2 }).n.of.^name, 'Int', 'element type survives the cached store';

# repeated stores in a loop through one site
class Loop {
    has %!h;
    method put(%x) { %!h = %x; self }
    method h() { %!h }
}
my $l = Loop.new;
$l.put(%(n => $_)) for ^5;
is $l.h<n>, 4, 'repeated stores through one site';

# a role-owned attribute
role R {
    has %!r;
    method put(%x) { %!r = %x; self }
    method r() { %!r }
}
class WithR does R { }
is WithR.new.put({ q => 1 }).r<q>, 1, 'role attribute stores';
is-deeply Loop.new.put({}).h, {}, 'storing an empty hash';
