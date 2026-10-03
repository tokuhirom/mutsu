use Test;

# A parametric role whose parameters all have defaults puns to its default
# parameterization; the pun is named after the role, not after the filled-in
# arguments (#11281).

plan 10;

role E[::R = Any] { has R $.v; method t { R.^name } }

is E.new.^name, 'E', '.^name of a pun built by .new';
is E.new(v => 1).raku, 'E.new(v => 1)', '.raku of that pun';
is E.new.WHAT.gist, '(E)', '.WHAT.gist of that pun';
ok E.new.WHAT === E.new.WHAT, 'every default pun is the same class';
is E.new(v => 3).v, 3, 'the pun still has the role attribute';
is E.t, 'Any', 'a method call on the role binds the default';
ok E.new ~~ E, 'the pun does the role';
is E[Int].new.^name, 'E[Int]', 'an explicit parameterization keeps its arguments in the name';

role F[::T = Int, $n = 3] { method d { self.WHAT.raku ~ ' ' ~ T.^name ~ $n } }
is F.d, 'F Int3', 'a method call puns to the role name with every default bound';
is F.new.^name, 'F', 'several defaulted parameters';
