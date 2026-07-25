unit module NativeNameSigilless;

# A sigilless parameter whose name coincides with a native type (`str`, `int`,
# `num`) must shadow that type inside the routine, exactly as a same-frame
# sigilless binding does. These live in a module because the bug only appeared
# for a routine compiled in a separate compilation unit, where the body is
# compiled with the signature supplied through `enclosing_sigilless` rather than
# as same-frame `sigilless_locals`.

sub takes-str (Str \str) is export { "str=[{str}] name={str.^name}" }
sub takes-int (Int \int) is export { "int=[{int}] name={int.^name}" }
sub takes-num (Num \num) is export { "num=[{num}] name={num.^name}" }
sub takes-other (Str \zzz) is export { "zzz=[{zzz}] name={zzz.^name}" }

# The String::Rotate shape: a coercion type, a defaulted second sigilless param,
# and a return constraint.
sub rot (Str(Any) \str, Int \ch = 1 --> Str) is export {
    my \shft = abs(ch % str.chars);
    str.substr(shft) ~ str.substr(0, shft)
}

# The type name must still resolve where no binding shadows it.
sub type-still-visible is export { str.^name }
