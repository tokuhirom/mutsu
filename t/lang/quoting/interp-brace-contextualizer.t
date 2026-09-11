use Test;

# `${...}` is ambiguous: the Perl 5 scalar dereference (X::Obsolete) and Raku's
# item contextualizer applied to the `{...}` circumfix. Rakudo's
# `special_variable:sym<${ }>` is guarded so the diagnosis never swallows the
# Raku form -- the braces are a composer when their text (up to the first `}`)
# holds `=>`, `:` + alpha, or `|%`, or is blank.
#
# mutsu applied that guard outside strings only, so `"${:group("MyGroup")}"`
# died with the P5 diagnosis mid-file. Found in vCard::Parser 0.0.2's
# t/03-actions.rakutest, which is exactly this line.

plan 14;

# --- the contextualizer, interpolated ---------------------------------------

is "${:group("MyGroup")}", "group\tMyGroup",
    'a colonpair composer interpolates as the itemized hash';
is "${a => 1}", "a\t1", 'a fat-arrow composer interpolates too';
is "${}", '', 'empty braces are the empty hash, not a deref';
is "pre ${:x(5)} post", "pre x\t5 post", 'it composes with surrounding text';

# --- and outside a string, where it already worked ---------------------------

is ${:group("MyGroup")}.raku, '${:group("MyGroup")}',
    'the bare form is still the itemized hash';
is ${a => 1, b => 2}.raku, '${:a(1), :b(2)}', 'several pairs';

# `@{...}` is the same circumfix in list context.
is @{:a, :b}.elems, 2, '@{...} is the list contextualizer, not a subscript';
is-deeply @{:a, :b}.sort(*.key).list, (:a, :b), '... yielding the pairs themselves';

# Interpolation does not trigger on `@{`: rakudo leaves the `@` literal and
# interpolates `{...}` as an ordinary block.
is "@{:a}", '@a	True', 'an interpolated @{...} keeps a literal @';

# --- the Perl 5 deref is still diagnosed ------------------------------------

my $scalar = 1;
my @array = 1, 2;

sub thrown($src) {
    my $e;
    { EVAL $src; CATCH { default { $e = $_ } } }
    $e;
}

is thrown(Q[${$scalar}]).^name, 'X::Obsolete', 'bare ${$scalar} is still obsolete';
is thrown(Q["${$scalar}"]).^name, 'X::Obsolete', '... and so is the interpolated form';
is thrown(Q[@{@array}]).^name, 'X::Obsolete', 'bare @{@array} is still obsolete';
is thrown(Q["@{@array}"]).^name, 'X::Obsolete', '... and its interpolated form';
is thrown(Q["${1}"]).^name, 'X::Obsolete',
    'a digit in the braces is a P5ism, not a composer';

# vim: expandtab shiftwidth=4
