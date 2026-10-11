use Test;

# `$!attr := $_` in a TWEAK aliases the attribute to the method frame's topic.
# That alias must not outlive the frame: it used to be merged into the caller,
# after which `.&sub` calls whose body used `with` clobbered the caller's `$_`
# (#10310, seen through CSS::Module::Property).
plan 4;

class P {
    has $.d;
    submethod TWEAK(:$default) { $!d := $_ with $default; }
}

sub kind(Str:D $k) { with $k.index(':') { 1 } else { 2 } }

my $p = P.new(:default<x>);
is $p.d, 'x', 'the attribute is bound to the topic value';

my @seen;
for <a b> { my $t = .&kind; @seen.push: $_ }
is-deeply @seen, ['a', 'b'], 'the caller topic survives a .&sub call using with';

@seen = ();
for <a b> { kind($_); @seen.push: $_ }
is-deeply @seen, ['a', 'b'], 'and a plain call';

is-deeply <a b>.map({ my $t = .&kind; $_ }).list, ('a', 'b'), 'and inside map';
