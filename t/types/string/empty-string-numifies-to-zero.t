use Test;

# Raku numifies an empty (or whitespace-only) string to 0, so it compares equal
# to 0 rather than "not a number at all".

plan 8;

ok "" == 0, 'an empty string equals 0';
ok "  " == 0, 'a whitespace-only string equals 0';
ok "\t\n" == 0, 'a string of other whitespace equals 0';
nok "" != 0, 'and is not unequal to 0';
ok "" < 1, 'it orders below 1';
ok "" >= 0, 'it orders at 0';
is "" <=> 0, Same, 'three-way compares Same';
is +"", 0, 'prefix + agrees';
