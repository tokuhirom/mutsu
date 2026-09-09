use v6;
use Test;

plan 12;

# `|%h` in an argument list: every entry becomes a NAMED argument (ADR-0021 I4).
# Regression pin for the #7561 fast path in `exec_make_slip_op`, which builds the
# named Pair directly for a plain (Str-keyed) hash instead of minting a
# positional `ValuePair` and immediately renaming it.
sub named-only(:$a, :$b) { "a=$a b=$b" }
my %h = a => 1, b => 2;
is named-only(|%h), "a=1 b=2", '|%h binds hash entries as named arguments';

sub catch-all(*@p, *%n) { "p={+@p} n={%n.sort.map({ .key ~ '=' ~ .value }).join(',')}" }
is catch-all(|%h), "p=0 n=a=1,b=2", '|%h contributes no positionals';

# The slipped pairs must be the named flavour, with a Str key.
sub inspect(*%n) {
    my $p = %n.sort.head;
    "{$p.key.^name}/{$p.key}/{$p.value}"
}
is inspect(|%h), "Str/a/1", 'a slipped hash entry keeps a Str key';

# An empty hash slips nothing.
my %e;
is catch-all(|%e), "p=0 n=", '|%h on an empty hash adds no arguments';

# Values are read through their container cell, not passed as the cell.
my %cont = n => 5;
sub takes-int(Int :$n) { $n + 1 }
is takes-int(|%cont), 6, 'a slipped value is decontainerized';

my @arr = 1, 2, 3;
my %withlist = list => @arr;
sub takes-list(:@list) { @list.join("-") }
is takes-list(|%withlist), "1-2-3", 'a slipped @-valued entry arrives as a list';

# Slipping does not consume or modify the source hash.
is %h.elems, 2, 'the slipped hash is unchanged';
is %h<a>, 1, 'the slipped hash still holds its values';

# A slip of a hash mixed with explicit named arguments; the explicit one wins
# when it comes later, matching Rakudo's last-wins rule for duplicate nameds.
sub pick(:$a, :$b) { "$a/$b" }
is pick(|%h, :a(9)), "9/2", 'an explicit named after |%h overrides it';

# `|%h` also works through a method call and through `.bless`.
class Holder {
    has $.x;
    has $.y;
    method build-from(%args) { self.new(|%args) }
}
my %args = x => "X", y => "Y";
my $h = Holder.new(|%args);
is $h.x ~ $h.y, "XY", '|%h works as a constructor argument list';
my $h2 = Holder.new(:x(1), :y(2)).build-from(%args);
is $h2.x ~ $h2.y, "XY", '|%h works from inside a method';

# An object hash keeps its typed keys through the general (non-fast) path.
my %oh{Int} = (1 => "one");
is %oh.keys.head.^name, "Int", 'an object hash still reports typed keys';

done-testing;
