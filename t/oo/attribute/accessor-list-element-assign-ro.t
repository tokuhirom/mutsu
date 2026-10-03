use Test;

# From Markdown::Lex: an `@` attribute rebound to an immutable List
# (`@!a := @!a.List`) must refuse element assignment through its accessor.
plan 4;

class T {
    has @.a;
    submethod TWEAK { @!a := @!a.List }
}
my $t = T.new(a => <p q>);
isa-ok $t.a, List, 'accessor returns a List';
throws-like { $t.a[0] = 'r' }, X::Assignment::RO, 'element assignment is refused';
throws-like { $t.a.[1] = 's' }, X::Assignment::RO, 'dotted subscript too';
is-deeply $t.a, <p q>.List, 'and the attribute is unchanged';
