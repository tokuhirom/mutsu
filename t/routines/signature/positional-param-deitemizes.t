use Test;

plan 9;

# An `@`-sigiled parameter is a positional binding: an *itemized* Positional
# argument (`$(1,2)` — what a list-assignment destructure leaves in a scalar)
# de-itemizes, so its elements become the array's elements. mutsu stored the
# itemized value straight through on the attributive form
# `submethod BUILD(:@!elems)`, leaving the attribute holding the list itself, so
# iterating it yielded that list instead of its elements.

class Elem { has $.v }

class WithBuild {
    has Elem @.elems;
    submethod BUILD(:@!elems) {}
}

sub names(@seq) { @seq.map(*.^name).join(',') }

my $itemized = (Elem.new(v => 'x'), Elem.new(v => 'y'));

my $from-scalar = WithBuild.new(elems => $itemized);
is $from-scalar.elems.elems, 2, 'the attribute has both elements';
is names($from-scalar.elems), 'Elem,Elem', 'iterating the attribute yields the elements';

# The same shape the YAMLish actions use: destructure, then pass on as `:$name`.
my ($cls, $elems) = (WithBuild, (Elem.new(v => 'x'), Elem.new(v => 'y')));
my $destructured = $cls.new(:$elems);
is names($destructured.elems), 'Elem,Elem', 'a destructured scalar binds the same way';

# A plain (non-itemized) list and an array variable were already correct.
is names(WithBuild.new(elems => (Elem.new(v => 'x'), Elem.new(v => 'y'))).elems),
    'Elem,Elem', 'a literal list still binds elementwise';
my @arr = Elem.new(v => 'x'), Elem.new(v => 'y');
is names(WithBuild.new(elems => @arr).elems), 'Elem,Elem', 'an array variable still binds elementwise';

# A plain `@` parameter (no attribute twigil) is unchanged.
sub takes(:@a) { names(@a) }
is takes(a => $itemized), 'Elem,Elem', 'a plain :@a parameter de-itemizes too';

# Assigning an itemized list to a positional attribute WITHOUT a BUILD keeps the
# item — that is a plain assignment, not a positional binding.
class NoBuild { has @.e }
is NoBuild.new(e => $itemized).e.elems, 1, 'plain attribute assignment still keeps the item';

# De-itemizing must preserve the shape: `$[…]` is an Array and `$(…)` a List, so
# `.raku` still renders what the argument was (roast/S06-currying/positional.t
# compares `@expect` bound from a `$[…]` against a plain Array with `eqv`).
sub shape(@a) { @a.raku }
is shape($['he']), ['he'].raku, 'an itemized Array de-itemizes to an Array';
is shape($('he',)), ('he',).raku, 'an itemized List de-itemizes to a List';
