use Test;

# A role attribute's default runs in the role's own declaration scope. That
# scope is captured once and shared by every class that consumes the role and
# every object it constructs (#12981): each must still see the role scope's
# lexicals, and see their current values.

plan 7;

my $counter = 0;
my $label = 'role-scope';

role Tagged {
    has $.tag = "$label-{++$counter}";
    has @.items = $label, $counter;
}

class A does Tagged { }
class B does Tagged { }
class C is A { }

is A.new.tag, 'role-scope-1', 'first construction sees the role scope';
is B.new.tag, 'role-scope-2', 'a second consuming class shares the counter';
is C.new.tag, 'role-scope-3', 'a subclass of a consumer sees it too';
is-deeply A.new.items, ['role-scope', 4], 'a container default sees the scope';

$label = 'changed';
is A.new.tag, 'changed-5', 'a later write to a captured lexical is seen';

is A.new(tag => 'given').tag, 'given', 'an explicit value skips the default';

# Many constructions in a loop keep producing fresh defaults.
my @tags = (^5).map({ B.new.tag });
is-deeply @tags, [<changed-6 changed-7 changed-8 changed-9 changed-10>],
    'repeated construction evaluates the default each time';
