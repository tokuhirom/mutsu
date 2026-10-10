unit class UnitConst::A;
my constant COLORS is export(:colors) = %( a => 1 );
my constant PLAIN = 'plain-a';
our constant DEFAULT is export = 'default-a';
method data { COLORS }
method plain { PLAIN }
