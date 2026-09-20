use Test;

plan 1;

module LexicalClassCoercionTest {
    my class LocalInt {
        has Int $.value is required;

        method new(Int:D $value) {
            self.bless(:$value)
        }
    }

    our sub make(Int:D $value) {
        LocalInt($value)
    }
}

is LexicalClassCoercionTest::make(7).value, 7,
    'a lexical class in a loaded package can be called as a coercion';
