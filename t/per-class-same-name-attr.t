use Test;

plan 8;

# A same-named attribute redeclared in a child class gets its OWN private
# storage: `$o.Parent::x = v` must touch only the parent's copy, leaving the
# child's intact, and each class's `$!x` resolves to its own. Mirrors
# roast/S12-methods/accessors.t subtests 8-10.
class Parent {
    has $.x is rw;
    method parent-x() { $!x }
}
class Child is Parent {
    has $.x is rw;
    method child-x() { $!x }
}

my $o = Child.new(x => 42);
# Both classes' BUILD bind their own x from the named arg.
is $o.parent-x, 42, 'parent copy initialized from named arg';
is $o.child-x,  42, 'child copy initialized from named arg';
is $o.x,        42, 'public accessor reads the most-derived copy';

$o.Parent::x = 5;
is $o.parent-x, 5,  'qualified assign updates the parent copy';
is $o.child-x,  42, 'child copy is untouched by Parent:: assign';
is $o.x,        42, 'public accessor still reads the child copy';

# Sanity: a plain single-class qualified rw accessor assign still works.
class Solo { has $.y is rw }
my $s = Solo.new(y => 1);
lives-ok { $s.Solo::y = 9 }, 'single-class qualified rw assign lives';
is $s.y, 9, 'single-class qualified rw assign updates the attribute';
