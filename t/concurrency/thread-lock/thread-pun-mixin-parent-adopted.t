use Test;
# A `but` mixin over a parametric-role pun that another thread made: the
# thread that constructs from the mixin composes the pun's published recipe
# for the parent instead of reporting it as an unknown class (Monad's cached
# `pun but Marker` types). Output identical under rakudo.
plan 8;

role Marker { method mark { "marked" } }
role Box[::T = Any] {
    has $.v;
    method make(\v) { self.new(:v(v)) };
    method who { "box" }
}

my $t;
Thread.start({ $t = Box.^pun but Marker }).finish;
is $t.^name, 'Box+{Marker}', 'defaulted pun mixin from another thread';
is $t.who, 'box', 'method on the mixin type object';
my $o = $t.make(3);
is $o.mark, 'marked', 'constructed object has the marker role';
is $o.v, 3, 'constructed object keeps its attribute';
ok $o ~~ Marker && $o ~~ Box, 'smartmatches both roles';

my $param;
Thread.start({ $param = Box.^parameterize(Int) }).finish;
my $mixed;
Thread.start({ $mixed = $param.^pun but Marker }).finish;
is $mixed.^name, 'Box[Int]+{Marker}', 'explicit pun mixin from other threads';
is $mixed.make(4).v, 4, 'explicit pun mixin constructs';
is $mixed.make(4).mark, 'marked', 'explicit pun mixin has the marker';
