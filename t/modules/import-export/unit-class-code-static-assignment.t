use Test;

# Regression from Timezones::ZoneInfo::State (Timezone::Simple).
plan 1;

unit class UnitClassCodeStaticAssignment;
my &helper;
&helper = sub { 42 };

ok True, 'a unit class can assign a declared code static';
