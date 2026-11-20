use Test;

# Non-constant parameter defaults bound by the method fast binder's default
# prologue (#18320): evaluated in the method frame, in parameter order, with
# `self`, attributes and earlier parameters visible.

plan 24;

my @log;
sub note-default($what) { @log.push($what); $what }

class C {
    has $.size = 3;
    method package { C }
    method m(Mu $x is raw, Mu :$type is raw = self.package, Bool :$flag) {
        $type.^name ~ ':' ~ ($flag // 'none')
    }
    method plain($a, :$type = self.package) { $type.^name ~ $a }
    method attr($n = $!size) { $n * 2 }
    method earlier($a, $b = $a + 1, :$c = $b * 10) { "$a $b $c" }
    method order($p = note-default('p'), :$q = note-default('q'), :$r = note-default('r')) {
        "$p$q$r"
    }
    method copied($n is copy = $!size) { $n += 10; $n }
    method typed(Int :$n = $.size) { $n }
    method word { 'str' }
    method bad(Int :$n = self.word) { $n }
    method dies(:$n = die('default died')) { $n }
    method topic(:$t = $_) { $t.^name }
}

my $c = C.new;
my $v = 1;

is $c.m($v, :flag), 'C:True', 'named is raw default reads self';
is $c.m($v, :type(Int)), 'Int:none', 'a supplied named is raw argument still wins';
is $c.plain('x'), 'Cx', 'plain named default reads self';
is $c.attr, 6, 'positional default reads an attribute';
is $c.attr(5), 10, 'supplied positional skips the default';
is $c.earlier(1), '1 2 20', 'defaults see earlier parameters';
is $c.earlier(1, 5), '1 5 50', 'a later default sees a supplied earlier parameter';

@log = ();
is $c.order, 'pqr', 'defaults bind in parameter order';
is @log.join, 'pqr', 'defaults are evaluated in parameter order';

is $c.copied, 13, 'an is copy default is the method\'s own writable container';
is $c.typed, 3, 'a typed default passes its type check';
throws-like { $c.bad }, Exception, 'a default failing its type check dies';
throws-like { $c.dies }, Exception, message => 'default died', 'a dying default fails the call';

# Repeated calls (the call plan replays) alternate omitted and supplied.
my @got;
for ^6 -> $i {
    @got.push: $i %% 2 ?? $c.plain($i) !! $c.plain($i, :type(Str));
}
is @got.join(','), 'C0,Str1,C2,Str3,C4,Str5', 'omitted and supplied defaults alternate correctly across replays';

my $sum = 0;
$sum += $c.attr for ^50;
is $sum, 300, 'a deferred default in a loop';

is $c.topic, 'Any', 'a default reading the topic sees the method\'s own fresh topic';

# A supplied `:$x is raw` aliases the caller's variable; a literal is read-only.
class R {
    method set(:$x is raw) { $x = 42 }
    method get(:$x is raw) { $x }
    method m(Mu $i is raw, Mu :$type is raw = Int) { $type.^name }
    method relay(Mu $type is raw) { self.m(1, :$type) }
}
my $r = R.new;
my $rv = 1;
$r.set(:x($rv));
is $rv, 42, 'a named is raw parameter writes through to the caller variable';
my @zs;
for ^3 { my $z = 0; $r.set(:x($z)); @zs.push($z) }
is @zs.join(','), '42,42,42', 'a fresh caller variable per iteration is aliased each time';
is $r.get(:x(7)), 7, 'a named is raw parameter reads a literal';
dies-ok { $r.set(:x(7)) }, 'a named is raw parameter bound to a literal is read-only';
is (^3).map({ $r.relay(Str) }).join(','), 'Str,Str,Str', 'a raw parameter relayed as a named is raw argument';

# AttrX::Mooish's `invoke-opt` shape: a typed constant default the fast binder
# cannot fill, and an enum default on an `is copy` parameter.
my enum PvtMode <pvmForce pvmNever pvmAsAttr pvmAuto>;
class I {
    method io(Mu $inv, Str $opt, Capture:D $params = \(), :$strict = False,
              PvtMode :$private is copy = pvmAuto) {
        $private = pvmAsAttr if $private == pvmAuto;
        "$opt {$params.elems} $strict $private"
    }
}
my $io = I.new;
is (^3).map({ $io.io(1, 'b') }).join('|'), 'b 0 False pvmAsAttr|b 0 False pvmAsAttr|b 0 False pvmAsAttr',
    'a Capture:D and an enum is copy default, repeatedly';
is $io.io(1, 'f', \(1, 2), :strict, :private(pvmNever)), 'f 2 True pvmNever', 'supplied arguments win';
throws-like { $io.io(1, 'x', \(), :private(5)) }, Exception, 'a supplied argument is still type-checked';
