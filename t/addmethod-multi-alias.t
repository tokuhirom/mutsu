use v6;
use Test;

# `^find_method` on a multi method returns a carrier for the whole candidate
# family; registering it under a new name via `^add_method` must alias ALL
# candidates, not freeze the alias to the first one's signature.
# (Text::CSV's BEGIN-time `alias` helper: `column-names` -> the four-candidate
# `column_names` multi.)

plan 6;

class Aliased {
    has @!names;
    multi method cn (Bool:D $b where *.not) { @!names = (); }
    multi method cn (Any:U) { @!names = (); }
    multi method cn (*@c) {
        @c.elems and @!names = @c.map(*.Str);
        @!names;
    }
}

BEGIN {
    my $r := Aliased.^find_method("cn");
    Aliased.^add_method("cn-alias", $r);
}

my $a = Aliased.new;
is $a.cn.elems, 0, 'original multi: zero-arg call hits the slurpy candidate';
is $a.cn-alias.elems, 0, 'alias: zero-arg call still dispatches (slurpy candidate)';
is $a.cn-alias("x", "y").elems, 2, 'alias: setter candidate stores values';
is $a.cn-alias.elems, 2, 'alias: state visible through zero-arg getter';
is-deeply $a.cn-alias(False), [], 'alias: Bool:D candidate clears';
is $a.cn.elems, 0, 'original sees the cleared state';
