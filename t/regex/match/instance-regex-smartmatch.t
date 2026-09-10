use v6;
use Test;

plan 6;

# Smartmatching an object against a regex stringifies via the user `Str`
# method (URI::Path smartmatched against the path grammar).
class P {
    has $.s;
    multi method Str(P:D: --> Str) { $!s }
}

my $p = P.new(s => '/a/b/c');
ok $p ~~ /a/, 'instance with user Str matches a regex';
nok $p ~~ /zz/, 'non-matching pattern fails';
ok $p ~~ m/'/a/'/, 'm// form stringifies too';
is ($p ~~ /(\w)/)[0].Str, 'a', 'captures come from the Str-ified text';

# :g form
is ($p ~~ m:g/\w/).elems, 3, ':g matches against the Str-ified text';

# Instances without a user Str keep prior behavior (no dispatch crash).
class NoStr { has $.v }
nok NoStr.new(v => 1) ~~ /abc/, 'instance without Str still smartmatches quietly';
