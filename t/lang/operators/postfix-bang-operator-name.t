use lib 't/lib';
use Test;
use PostfixBangName;

plan 3;

sub postfix:<!zz>($value) { "local $value" }

is 4!zz, 'local 4', 'a declared local !name postfix wins over private-method parsing';
is 5!mx, 'imported 5', 'an imported !name postfix wins over private-method parsing';

class PrivateBangMethod {
    method !secret() { 42 }
    method call-secret() { self!secret() }
}
is PrivateBangMethod.new.call-secret, 42, 'private method calls remain available';
