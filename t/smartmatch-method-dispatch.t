use Test;

plan 6;

# `$obj ~~ Pair` calls the method named by the pair key and compares its
# boolean coercion with the pair value's. This exercises the VM's unified
# compiled-first method dispatch for the smartmatch key-method path
# (vm_smart_match.rs), using a non-accessor user-defined method.
class N {
    has $.n;
    method even  { $.n %% 2 }
    method big   { $.n > 100 }
}

my $four = N.new(n => 4);
my $five = N.new(n => 5);

ok  ($four ~~ :even(True)),  'user method via smartmatch pair: even(4) is True';
ok  ($five ~~ :even(False)), 'user method via smartmatch pair: even(5) is False';
ok  ($four ~~ :even),        ':even shorthand (=> True) matches even(4)';
nok ($five ~~ :even),        ':even shorthand does not match odd(5)';

my $huge = N.new(n => 9999);
ok  ($huge ~~ :big),         'second user method (big) dispatches correctly';
nok ($four ~~ :big),         'big(4) is False';
