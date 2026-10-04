use Test;

# A contextualizer's code can contain another slash-delimited regex. Its
# slashes belong to that regex, not the enclosing pattern (#11606).
plan 5;

ok 'ab' ~~ / $( rx/ a / ) b /, 'nested rx// in $(...)';
ok 'ab' ~~ / @( rx/ a / ) b /, 'nested rx// in @(...)';
ok 'ab' ~~ rx{ $( rx/ a / ) b }, 'a paired outer delimiter';
ok 'ab' ~~ / $( rx/ "a" / ) b /, 'quoted text in the nested regex';
ok 'x)y' ~~ / x $(")") y /, 'a quoted parenthesis stays inside the contextualizer';
