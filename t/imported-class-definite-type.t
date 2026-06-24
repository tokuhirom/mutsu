use v6;
use lib $?FILE.IO.parent.add('lib');
use Test;
use ImportedDefiniteType;

plan 6;

# An imported class used as a `:D`-smiley type constraint on a `my` variable
# must be recognized as a type (it is registered under its qualified name
# `ImportedDefiniteType::Widget` while the importing scope refers to it by the
# short alias `Widget`). Regression: this threw
# X::Syntax::Variable::BadType "Package 'Widget:D' is insufficiently type-like".
{
    my Widget:D $w = Widget.new(size => 3);
    is $w.area, 9, 'my ImportedClass:D in main scope';
}

# The short name is an alias to the qualified class.
is Widget.^name, 'ImportedDefiniteType::Widget', 'imported class keeps its qualified name';

# The same `my ImportedClass:D` inside a method that is dispatched (and thus
# compiled) lazily — the Humming-Bird Route.CALL-ME shape.
class Holder {
    has &.cb is required;
    method CALL-ME($given?) {
        my Widget:D $w = $given // Widget.new(size => 4);
        &!cb($w);
    }
}
{
    my $h = Holder.new(cb => -> $w { $w.area });
    is $h(), 16, 'my ImportedClass:D inside a CALL-ME method';
    is $h(Widget.new(size => 5)), 25, 'CALL-ME with an explicit instance';
}

# `:D` enforcement still applies (a type object is rejected).
{
    my $threw = False;
    { my Widget:D $w = Widget; CATCH { default { $threw = True } } }
    ok $threw, ':D smiley still rejects a type object for an imported class';
}

# A genuinely-unknown type is still rejected.
{
    my $threw = False;
    { ::('NoSuchType123'); my $x; CATCH { default { $threw = True } } }
    # (the ::() lookup itself throws for an undeclared type)
    ok $threw, 'unknown type name still errors';
}
