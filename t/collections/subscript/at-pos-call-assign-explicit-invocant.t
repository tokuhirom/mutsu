use Test;

plan 10;

# `$o.AT-POS($i) = $v` is a call whose rw result is assigned to, never the
# element store `$o[$i] = $v`. Rewriting it to the latter made a user
# ASSIGN-POS that is spelled `SELF.AT-POS($index) = $value` (Red's
# ColumnMethods, with an explicit invocant) call itself forever.

my @stored;
class Col {
    has $.returns = Int;
    method AT-POS(\SELF where { .returns ~~ Int }: $key where { $_ ~~ Int }) is rw {
        Proxy.new(FETCH => -> $ { 0 }, STORE => -> $, $v { @stored.push: "$key=$v" });
    }
    method ASSIGN-POS(\SELF where { .returns ~~ Int }: $index, $value) {
        SELF.AT-POS($index) = $value
    }
}
my $c = Col.new;
$c[1] = 5;
is @stored.join(","), "1=5", 'a user ASSIGN-POS assigning through SELF.AT-POS terminates';
$c.AT-POS(2) = 7;
is @stored.join(","), "1=5,2=7", 'an explicit AT-POS call assigns through the returned Proxy';

class Plain {
    method AT-POS($key) is rw { Proxy.new(FETCH => -> $ { 0 }, STORE => -> $, $v { @stored.push: "plain $key=$v" }) }
    method ASSIGN-POS(Plain:D $self: $index, $value) { $self.AT-POS($index) = $value }
}
Plain.new[3] = 9;
is @stored.tail, "plain 3=9", 'an explicit $self invocant behaves the same';

my @a = 1, 2, 3;
@a.AT-POS(1) = 20;
is-deeply @a, [1, 20, 3], 'an Array element is assigned through AT-POS';
my %h = a => 1;
%h.AT-KEY("b") = 2;
is %h<b>, 2, 'a Hash element is assigned through AT-KEY';
is %h<a>, 1, '... without touching the others';
@a.AT-POS(5) = 6;
is @a.elems, 6, 'assigning past the end extends the Array';

my $s = (gather { take 1; take 2 });
throws-like { $s.AT-POS(0) = 5 }, X::Assignment::RO, 'a bare gather element refuses the store';
my @b = 1, 2, 3;
@b.AT-POS(0) += 10;
is @b[0], 11, 'a compound assignment through AT-POS';
is (@b.AT-POS(2) = 33), 33, 'the assignment expression answers the value';
