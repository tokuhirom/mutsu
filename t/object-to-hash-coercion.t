use Test;

# An object with a hash-like interface (via `handles`) bound to a `%`-sigil
# target is list-contextualized like Raku's `Hash.STORE`: its pairs are
# materialized into a real Hash instead of the object being kept verbatim.

plan 12;

class HashLike {
    has %.h handles <AT-KEY EXISTS-KEY keys values kv iterator list>;
}

# --- `has %.attr` bound to a hash-like object ------------------------------
class Wrapper {
    has %.config;
}

my $obj = HashLike.new(h => { a => 1, b => 2 });
my $w = Wrapper.new(config => $obj);

is $w.config.^name, 'Hash', 'object bound to %.attr becomes a Hash';
is $w.config<a>, 1, 'first pair materialized into the %.attr hash';
is $w.config<b>, 2, 'second pair materialized into the %.attr hash';

# --- plain `my %h = $obj` --------------------------------------------------
my %m = $obj;
is %m.^name, 'Hash', 'my %h = $obj coerces to a Hash';
is %m<a>, 1, 'my %h = $obj materializes first pair';
is %m<b>, 2, 'my %h = $obj materializes second pair';

# --- a real Hash / array-of-pairs still coerces as before ------------------
is Wrapper.new(config => { p => 9 }).config<p>, 9, 'plain Hash still binds';
is Wrapper.new(config => (q => 8, r => 7)).config<r>, 7, 'array of pairs still binds';
is Wrapper.new.config.^name, 'Hash', 'unspecified %.attr defaults to empty Hash';

# --- anonymous class, mirroring zef's $CONFIG wrapper ----------------------
my $cfg = class :: {
    has %.hash handles <AT-KEY EXISTS-KEY keys values kv iterator list>;
}.new(hash => { Fetch => [1, 2], Repository => [3] });

my $c = Wrapper.new(config => $cfg);
is $c.config.^name, 'Hash', 'anon-class config wrapper becomes a Hash';
is $c.config<Fetch>.elems, 2, 'anon-class hash value preserved (elems)';
is $c.config<Repository>[0], 3, 'anon-class hash value preserved (indexing)';
