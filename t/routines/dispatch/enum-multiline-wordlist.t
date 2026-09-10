use Test;

plan 6;

# Multi-line `<...>` word-quote enum variant lists (newlines between words).
my enum Dir <
    North East
    South West
>;
is North.value, 0, 'first variant on continued line';
is West.value,  3, 'last variant after newline';

# Anonymous enum spanning lines.
enum <
    Red
    Green Blue
>;
is Red.value,   0, 'anon enum first variant';
is Blue.value,  2, 'anon enum variant after newline';

# The single-line form must still work.
my enum Flag <a b c>;
is a.value, 0, 'single-line enum still works';
is c.value, 2, 'single-line enum last variant';
