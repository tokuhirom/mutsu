# Needle::Compile (#15087): a multi whose parameter type is a module-private `my constant`.
my role Type { has $.type; method Str (Type:D:) { self ~ "" } }
my constant StrType = Str but Type;
my proto sub kind(|) {*}
my multi sub kind(StrType:D $_) { "mixed:" ~ .type }
my multi sub kind(Str:D $_)     { "plain" }
my sub EXPORT(*@names) {
    my constant %valid = '&kind' => &kind, 'Type' => Type, 'StrType' => StrType;
    Map.new: @names.map: { (%valid{"&$_"}:exists) ?? (%valid{"&$_"}:p) !! (%valid{$_}:p) }
}
