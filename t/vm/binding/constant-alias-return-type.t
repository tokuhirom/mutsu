use Test;

plan 2;

constant lowercase-return-type-alias = int64;

sub returns-native-alias(--> lowercase-return-type-alias) {
    return 42;
}

is returns-native-alias(), 42,
    'a lowercase constant type alias is accepted as a return type';

sub stores-native-alias(--> lowercase-return-type-alias) {
    my lowercase-return-type-alias $value = 42;
    $value
}

is stores-native-alias(), 42,
    'a hoisted sub compiles a local using a lowercase type alias';
