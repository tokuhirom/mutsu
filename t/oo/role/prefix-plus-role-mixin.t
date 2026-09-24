use v6;
use Test;

plan 1;

role Rule {
    has Int $.number is required;

    method Numeric(--> Int:D) {
        $.number
    }
}

my Rule $rule .= new(:number(30));
is +$rule, 30, 'prefix + dispatches Numeric on a punned role';
