use Test;

# Date.raku / DateTime.raku render the numeric-argument constructor form
# (`Date.new(2020,3,5)`, `DateTime.new(2020,3,5,13,45,30)`), which round-trips
# through .EVAL — not the ISO-string form. `.Str` / `.gist` keep the ISO form.

plan 14;

is Date.new(2020, 1, 1).raku,    'Date.new(2020,1,1)',   'Date.raku numeric form';
is Date.new(2020, 12, 5).raku,   'Date.new(2020,12,5)',  'Date.raku two-digit fields';
is Date.new(2020, 1, 1).perl,    'Date.new(2020,1,1)',   'Date.perl matches .raku';
# .raku built from a string constructor still renders numeric
is Date.new("2020-03-05").raku,  'Date.new(2020,3,5)',   'Date.raku from string ctor is numeric';
# .Str / .gist keep ISO form
is Date.new(2020, 3, 5).Str,     '2020-03-05',           'Date.Str keeps ISO form';
is Date.new(2020, 3, 5).gist,    '2020-03-05',           'Date.gist keeps ISO form';

is DateTime.new(2020, 3, 5, 13, 45, 30).raku,
    'DateTime.new(2020,3,5,13,45,30)',                   'DateTime.raku numeric form';
is DateTime.new(2020, 3, 5, 13, 45, 30.5).raku,
    'DateTime.new(2020,3,5,13,45,30.5)',                 'DateTime.raku fractional second';
is DateTime.new(2020, 3, 5, 13, 45, 30, :timezone(3600)).raku,
    'DateTime.new(2020,3,5,13,45,30,:timezone(3600))',   'DateTime.raku with timezone';
is DateTime.new(2020, 1, 1, 0, 0, 0).raku,
    'DateTime.new(2020,1,1,0,0,0)',                      'DateTime.raku midnight';
is DateTime.new(2020, 3, 5, 13, 45, 30).Str,
    '2020-03-05T13:45:30Z',                              'DateTime.Str keeps ISO form';

# round-trips through .EVAL
my $d = Date.new(2020, 7, 14);
ok $d.raku.EVAL eqv $d, 'Date.raku.EVAL round-trips';
my $dt = DateTime.new(2020, 7, 14, 9, 8, 7);
ok $dt.raku.EVAL eqv $dt, 'DateTime.raku.EVAL round-trips';
my $dtz = DateTime.new(2020, 7, 14, 9, 8, 7, :timezone(3600));
ok $dtz.raku.EVAL eqv $dtz, 'DateTime.raku.EVAL with timezone round-trips';
