use v6;
use Test;

plan 10;

# A user-defined Associative/Positional object in a scalar dispatches the
# raku subscript protocol for writes and deletes (URI::Query's shape).
class K does Associative {
    has @.pairs;
    method AT-KEY($k) { @!pairs.first({ .key eq $k }).?value }
    method ASSIGN-KEY($k, $v) {
        @!pairs .= grep({ .key ne $k });
        @!pairs.push($k => $v);
    }
    method DELETE-KEY($k) {
        my $old = self.AT-KEY($k);
        @!pairs .= grep({ .key ne $k });
        $old;
    }
}
class P does Positional {
    has @.pairs;
    method AT-POS($i) { @!pairs[$i] }
    method ASSIGN-POS($i, Pair $p) { @!pairs[$i] = $p }
    method DELETE-POS($i) {
        my $old = @!pairs[$i];
        @!pairs.splice($i, 1);
        $old;
    }
}

my $k = K.new;
$k<a> = 1;
is $k<a>, 1, 'ASSIGN-KEY dispatches (instance not clobbered by a Hash)';
is $k.pairs.elems, 1, 'the instance kept its own storage';

$k<b> = 2;
my $old = $k<a>:delete;
is $old, 1, ':delete dispatches DELETE-KEY and returns the old value';
is $k.pairs.elems, 1, 'DELETE-KEY removed the pair';

# Positional protocol, including a Pair VALUE as a positional argument.
my $p = P.new;
$p[0] = 'q' => 9;
is $p[0].key, 'q', 'ASSIGN-POS receives the Pair positionally';
my $removed = $p[0]:delete;
is $removed.value, 9, 'DELETE-POS dispatches and returns the element';

# Assignment through a method accessor returning the object.
class Holder {
    has K $.inner = K.new;
}
my $h = Holder.new;
$h.inner<x> = 'v';
is $h.inner<x>, 'v', 'method-accessor target dispatches ASSIGN-KEY';

# `@a[0] = "k" => 1` — item assignment is looser than the fat arrow.
my @a;
@a[0] = "k" => 1;
is-deeply @a[0], ("k" => 1), 'element assign takes the whole Pair as value';
my %h;
%h<x> = "k" => 1;
is-deeply %h<x>, ("k" => 1), 'hash element assign takes the whole Pair';
my @b;
@b[0] = k => 1;
is @b[0].key, 'k', 'bareword pair key auto-quotes in element assign';
