# A `sub EXPORT` module that installs values under the CORE term keyword names
# `True`/`False`, shadowing them lexically for the importing compunit -- the
# shape the `Logic::Ternary` ecosystem distribution uses to replace two-valued
# logic with its own three-valued objects.
#
# The exported names are computed at run time (`@names[0]`, defaulted but
# overridable by the `use` arguments), so no static scan of this file can know
# them: that is exactly what makes the hardcoded parser literals in
# `keyword_literal` unable to yield to it without a run-time check.
# https://github.com/tokuhirom/mutsu/issues/9047
use v6.d;

class Tri is export {
    has Int $.v;
    method Str { "Tri({$!v})" }
    method gist { self.Str }
    method Int { $!v }
}

sub EXPORT(*@options) {
    my @names = @options ?? @options.list !! <True False>;
    my %map;
    %map{@names[0]} = Tri.new(v => 1);
    %map{@names[1]} = Tri.new(v => -1);
    Map.new(|%map);
}
