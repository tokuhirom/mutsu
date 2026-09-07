use v6;
use Test;

# An ITEMIZED value used as a hash subscript is ONE key, not a slice:
#
#     my $s = $(1, 2);
#     my %c; %c{$s} = "x";   # {"1 2" => "x"}
#
# while the NON-itemized `%c{(1, 2)}` really does slice. Itemization is exactly
# what distinguishes the two. mutsu flattened every list-shaped hash subscript
# into a slice and, worse, numified an itemized one to its element COUNT — so
# `%c{$(1, 2)}` and `%c{$(3, 4)}` were the same key `"2"`.
#
# The positional side is unaffected and stays as it was: `@a[$(1, 2)]` IS a
# single numeric index in raku (the itemized list's `.Int`, its element count).
# Every expectation below was measured against rakudo 2026.07.

plan 17;

# --- the ticket's write repro ------------------------------------------
my $s = $(1, 2);
my %c;
%c{$s} = "x";
is %c.raku, '{"1 2" => "x"}', 'an itemized List subscript is one key';

my $a = $[3, 4];
my %d;
%d{$a} = "w";
is %d.raku, '{"3 4" => "w"}', 'an itemized Array subscript too';

# Two DIFFERENT itemized lists are two different keys — they used to collapse
# onto the same `"2"` (the element count).
my %e;
%e{$(1, 2)} = "a";
%e{$(3, 4)} = "b";
is %e.elems, 2, 'two itemized subscripts of the same length are two keys';
is %e{$(1, 2)}, 'a', 'and each reads back its own value';
is %e{$(3, 4)}, 'b', 'both of them';

# --- the read repro -----------------------------------------------------
my %f;
%f{"1 2"} = "y";
is %f{$s}, 'y', 'an itemized subscript reads the key the string spelling wrote';

# --- :exists and :delete take the same path ----------------------------
my %g;
%g{$s} = 1;
ok %g{$s}:exists, ':exists finds it';
is (%g{$s}:delete), 1, ':delete removes it';
is %g.raku, '{}', 'and the hash is empty afterwards';

# --- a NON-itemized list subscript still slices -------------------------
my %h;
%h{(1, 2)} = "z";
is %h.keys.sort.join(','), '1,2', 'a bare list subscript is still a slice';

my @k = <a b>;
my %i;
%i{@k} = "p", "q";
is %i.raku, '{:a("p"), :b("q")}', 'a slice from an @-array still slices';

# --- the positional side must not move ----------------------------------
my @arr = 10, 20, 30;
is @arr[$(1, 2)], 30, 'an itemized positional subscript is its element count';
is @arr[(1, 2)].raku, '(20, 30)', 'a bare list positional subscript still slices';
is @arr[$[0, 1, 2]], Any, 'an itemized Array positional subscript counts too';

# --- a key-constrained (object) hash accepts the whole value -----------
# It used to report the LAST ELEMENT's type ("expected List:D but got Int (2)"),
# which is the tell that the subscript had already been flattened.
my $t = $(1, 2);
my Any:D %j{List:D};
lives-ok { %j{$t} = 'x' }, 'a List:D-keyed hash accepts an itemized List subscript';
is %j.elems, 1, 'and stores exactly one entry';
is %j.keys[0].^name, 'List', 'whose key is the List itself, not a stringification';
