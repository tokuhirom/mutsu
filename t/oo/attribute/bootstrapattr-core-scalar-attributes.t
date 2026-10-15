use Test;

plan 14;

for Int, Num, Str -> $type {
    my $attribute = $type.^attributes[0];
    is $attribute.^name, 'BOOTSTRAPATTR', "{$type.^name} value attribute has BOOTSTRAPATTR identity";
    nok $attribute ~~ Attribute, "{$type.^name} value attribute is not an Attribute";
    nok $attribute ~~ Any, "{$type.^name} value attribute is not an Any";
    ok $attribute ~~ Mu, "{$type.^name} value attribute is a Mu";
}

is Rat.^attributes[0].^name, 'Attribute', 'ordinary built-in attributes remain Attribute instances';
dies-ok { for Int.^attributes -> Attribute $attribute { } },
    'a BOOTSTRAPATTR cannot bind to an Attribute parameter';
