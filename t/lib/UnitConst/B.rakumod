unit class UnitConst::B;
my constant COLORS is export(:colors) = %( b => 2, c => 3 );
my constant PLAIN = 'plain-b';
method data { COLORS }
method plain { PLAIN }
