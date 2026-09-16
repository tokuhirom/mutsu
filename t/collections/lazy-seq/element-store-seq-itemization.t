use Test;

plan 4;

# A still-deferred Seq stored into a Hash/Array element must keep its `Seq`
# identity (itemized on the SeqBody handle, like a plain `$x = SEQ` scalar
# assignment already did) rather than being wrapped in a generic `Scalar`
# container. The wrapper drops the tag that the reify-before-stringify guard
# in `coerce_stringy_operand` probes for, so a later `eq`/interpolation on
# the still-unreified Seq silently read the empty not-yet-pulled generation
# instead of reifying first — found via Net::Netmask's `enumerate(:nets)`
# results (a Seq built from `.map`) compared through the vendored
# `Test::is`, after being round-tripped through a Hash element exactly like
# `%h<k> = @a.map(...)`.

my %h;
%h<k> = <a b c>.map({ "{$_}-x" });
is %h<k>, "a-x b-x c-x", 'a Seq stored into a hash element stringifies/eq-compares correctly';

my @arr;
@arr[0] = <a b c>.map({ "{$_}-y" });
is @arr[0], "a-y b-y c-y", 'a Seq stored into an array element stringifies/eq-compares correctly';

# The same value read back must still actually equal the expected string via
# infix `eq` (not just via `is`, which has its own fallback formatting).
%h<k2> = (1, 2, 3).map({ $_ * 2 });
ok %h<k2> eq "2 4 6", 'infix eq on a hash-stored Seq reifies before comparing';

@arr[1] = (1, 2, 3).map({ $_ * 3 });
ok @arr[1] eq "3 6 9", 'infix eq on an array-stored Seq reifies before comparing';
