use Test;

# #9023: a Hash/Array assigned to a `$`-sigil attribute through its accessor
# was stored un-itemized. A `$` attribute IS a Scalar container, so what it
# holds renders with the itemizing `$` prefix and stops flattening under the
# single-argument rule -- exactly what `my $x = [1,2,3]` produces. The
# ordinary local-slot store gets that from `itemize_scalar_store`; the
# accessor store in `assign_method_lvalue_with_values` did not.
#
# `.raku` is the readable witness, but the itemization is not cosmetic: the
# single-argument-rule assertions below (`my @flat = $obj.x`) are the same
# flag observed as behaviour.

plan 16;

class Acc { has $.x is rw }

my $h = Acc.new;
$h.x = {a => 1, b => 2};
is $h.x.raku, '${:a(1), :b(2)}', 'a Hash assigned through the generated accessor is itemized';

my $a = Acc.new;
$a.x = [1, 2, 3];
is $a.x.raku, '$[1, 2, 3]', 'an Array assigned through the generated accessor is itemized';

class Inner { has $.x is rw; method set { $.x = {c => 3} } }
my $in = Inner.new;
$in.set;
is $in.x.raku, '${:c(3)}', 'a Hash assigned to `$.x` from inside a method is itemized';

class RwMethod { has $!w; method w is rw { $!w } }
my $rw = RwMethod.new;
$rw.w = {d => 4};
is $rw.w.raku, '${:d(4)}', 'a Hash assigned through an `is rw` method is itemized';

my $rwa = RwMethod.new;
$rwa.w = [7, 8];
is $rwa.w.raku, '$[7, 8]', 'an Array assigned through an `is rw` method is itemized';

# A second store must itemize too. This one used to take the in-place
# `store_into_attr_container` shortcut, which adopted the new contents into
# the destination container and so kept the destination's older tag. A `$`
# attribute is a Scalar: `=` rebinds it to the new container instead.
my $again = RwMethod.new;
$again.w = {a => 1};
$again.w = {b => 2};
is $again.w.raku, '${:b(2)}', 'a second Hash store through an `is rw` method is itemized';

my $again2 = Acc.new;
$again2.x = [1, 2];
$again2.x = [3, 4];
is $again2.x.raku, '$[3, 4]', 'a second Array store through the generated accessor is itemized';

# Rebinding, not refilling: an alias taken before the second store keeps the
# container it was given.
class Rebind { has $!w; method w is rw { $!w } }
my $rb = Rebind.new;
$rb.w = {a => 1};
my $alias = $rb.w;
$rb.w = {b => 2};
is $alias.raku, '${:a(1)}',
    'a `$` accessor store rebinds the attribute rather than refilling the old container';

# The itemization observed as behaviour, not just as `.raku` output.
my $flat = Acc.new;
$flat.x = [1, 2, 3];
my @flat = $flat.x;
is @flat.elems, 1, 'a `$`-held Array attribute contributes one element under the single-argument rule';
is $flat.x.elems, 3, 'the `$`-held Array attribute still has its three elements';

# `@`/`%` attributes are containers in their own right and must NOT itemize.
class Cont { has @.a is rw; has %.h is rw }
my $c = Cont.new;
$c.a = [1, 2, 3];
$c.h = {e => 5};
is $c.a.raku, '[1, 2, 3]', 'an `@` attribute assigned a list stays un-itemized';
is $c.h.raku, '{:e(5)}', 'a `%` attribute assigned a hash stays un-itemized';

# A `%` attribute's in-place refill (the `store_into_attr_container` rule this
# ticket narrowed to `@`/`%`) must still keep the container's identity.
class Refill { has %!h; method h { %!h } }
my $rf = Refill.new;
$rf.h = {a => 1};
my $same = $rf.h;
$rf.h = {b => 2};
# (`$same` is itself a `$` scalar, so its own `.raku` carries the `$` prefix;
# what this pins is that the alias SEES the new contents, i.e. the container
# was refilled rather than replaced.)
is $same.raku, '${:b(2)}',
    'a `%` accessor store still refills the existing container in place';

# Plain scalar values are unaffected.
my $sc = Acc.new;
$sc.x = 42;
is $sc.x.raku, '42', 'an Int assigned through the accessor is unchanged';
$sc.x = "str";
is $sc.x.raku, '"str"', 'a Str assigned through the accessor is unchanged';

# A lexical `$` has always itemized; pinned so a fix here cannot regress it.
my $lex = {a => 1, b => 2};
is $lex.raku, '${:a(1), :b(2)}', 'a lexical `$` still itemizes a Hash literal';
