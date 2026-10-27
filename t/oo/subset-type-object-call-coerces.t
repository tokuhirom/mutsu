use Test;

# From XML::Class (t/090-types.t): `$type($text)` with a subset type object
# coerces through the refinee and checks the subset, as `Int("50")` would.
plan 6;

subset Short of Int where * < 100;
is Short("50"), 50, 'a subset name called as a function coerces its argument';
my $t = Short;
is $t("51"), 51, 'the subset type object held in a variable coerces too';
dies-ok { Short("500") }, 'a value outside the subset is refused';

class Holder {
    subset Small of Int where * < 10;
    has Small $.v;
    method make(Str $s) { Small($s) }
}
is Holder.make("7"), 7, 'a class-scoped subset coerces';
my $type = Holder.^attributes[0].type;
is $type("8"), 8, 'the type of a subset-typed attribute coerces';
dies-ok { $type("80") }, 'and refuses a value outside the subset';
