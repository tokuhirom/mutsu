use Test;

plan 4;

my \a = 0, ([\+] 1..*).Slip;
is a[5], 15, '.Slip works with scan reduction';

my \b = 0, Slip([\+] 1..*);
is b[5], 15, 'Slip() works with scan reduction';

my \c = 0, slip [\+] 1..*;
is c[5], 15, 'slip works with scan reduction';

my \d = 0, |[\+] 1..*;
is d[5], 15, 'prefix | works with scan reduction';
