unit module UnitModuleScope;

# Not exported, in three different scope declarations. None of these may be
# callable by their bare name from a consumer that merely `use`s this module.
sub plain-helper(Str $a) { "plain($a)" }
our sub our-helper(Str $a) { "our($a)" }
my sub my-helper(Str $a) { "my($a)" }

our $pkg-var = 42;

# The module's own routines still see their siblings by bare name.
sub call-all() is export {
    plain-helper('x') ~ ' ' ~ our-helper('x') ~ ' ' ~ my-helper('x')
}

# A `once` inside an exported module sub must fire exactly once across calls,
# whichever dispatch path (fresh resolve / cached OTF body) the caller takes.
my $once-count = 0;
sub bump-once() is export {
    once { $once-count++ }
    $once-count
}

proto pick(|) is export {*}
multi pick(Int $x) { "int($x)" }
multi pick(Str $x) { "str($x)" }
