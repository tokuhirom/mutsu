use Test;

# From Log::Async t/04-filter.rakutest: `:level(TRACE..INFO)` filters by an
# enum-valued Range smartmatched against an enum value.
plan 7;

enum L <TRACE DEBUG INFO WARNING ERROR FATAL>;

ok DEBUG ~~ TRACE..INFO, 'inner enum value matches enum Range';
ok INFO ~~ TRACE..INFO, 'upper bound matches';
ok TRACE ~~ TRACE..INFO, 'lower bound matches';
nok ERROR ~~ TRACE..INFO, 'value above the range does not match';
nok INFO ~~ TRACE..^INFO, 'excluded end does not match';
nok TRACE ~~ TRACE^..INFO, 'excluded start does not match';
ok (TRACE..INFO).ACCEPTS(DEBUG), '.ACCEPTS agrees';

done-testing;
