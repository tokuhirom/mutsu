use Test;
plan 8;

# Gnome::Gio::EmblemedIcon: `unit role X; also is Gnome::GObject::Object;`.
# `also is Parent` in a role body is the header's `role R is Parent`.
class Base { method b { "base" } }
role Marker { method m { "marker" } }

role Blocked {
    also is Base;
    also does Marker;
    has $.x;
    method hi { "hi {$.x}" }
}
my $o = Blocked.new(:x(5));
is $o.hi, 'hi 5', 'block role with also is: own method';
is $o.b, 'base', 'block role with also is: parent method';
is $o.m, 'marker', 'block role with also is: also does';
is $o.^mro.map(*.^name).join(' '), 'Blocked Base Any Mu', 'the parent is in the pun MRO';

class K does Blocked { }
is K.new(:x(1)).b, 'base', 'a class composing the role inherits the parent';
is K.^mro.map(*.^name).join(' '), 'K Base Any Mu', 'composing class MRO';

my $r = EVAL q:to/END/;
    unit role UnitRole;
    also is Base;
    method hi { "unit hi" }
    END
is $r.new.hi, 'unit hi', 'unit role with also is: own method';
is $r.new.b, 'base', 'unit role with also is: parent method';
