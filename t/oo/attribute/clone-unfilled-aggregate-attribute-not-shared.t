use Test;

# Data::Dump::Tree: `my $clone = self.clone(|args.hash)` followed by
# `@.kb_colors.append: ...` in the clone "so we do not pollute the object".
# An `@`/`%` attribute nobody has filled yet is not vivified in Rakudo, so the
# clone and the original each get their own container on first use. A filled
# one stays shared (t/oo/attribute/attribute-container-identity.t). mutsu
# shared the empty container too, and the clone's fill showed on the original.

plan 8;

class C {
    has @.a;
    has %.h;
    has @.filled = <x y>;
    has $.s is rw = 1;
    method fill { @!a.append: 1, 2; %!h<k> = 1; self }
}

my $orig = C.new;
my $copy = $orig.clone;
$copy.fill;
is $orig.a.elems, 0, 'an unfilled @-attribute is not shared with the clone';
is $orig.h.elems, 0, 'nor an unfilled %-attribute';
is $copy.a.elems, 2, 'the clone has its own contents';

my $with = C.new;
my $twiddled = $with.clone(:s(7));
$twiddled.fill;
is $with.a.elems, 0, 'also for a clone with a twiddle of another attribute';
is $with.h.elems, 0, 'the same for the hash';

ok $orig.filled =:= $copy.filled, 'a filled @-attribute is still shared';

class Holder { has $.items = []; }
my $h1 = Holder.new;
my $h2 = $h1.clone;
ok $h1.items =:= $h2.items, 'a scalar holding an array keeps sharing it';
$h2.items.push: 5;
is $h1.items.elems, 1, 'a push through the clone shows on the original';
