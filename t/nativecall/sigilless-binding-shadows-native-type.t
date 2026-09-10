use Test;

# A sigilless binding (`my \x`, or a `\x` / typed `Str \x` routine parameter)
# whose name coincides with a native lowercase type (`str`, `int`, `num`) must
# shadow that type within its scope: a bare reference to the name reads the
# binding, not the native type object. Previously the compiler resolved the
# bare word to the native type (`say str` printed `(str)`), ignoring the
# lexical binding.

plan 6;

{
    my \str = "hello";
    is str, "hello", 'my \str shadows the native str type';
}

{
    my \int = 42;
    is int, 42, 'my \int shadows the native int type';
}

sub take-str (\str) { str.chars }
is take-str("Rakudo"), 6, 'sigilless \str param shadows native type in body';

sub rotate-it (Str(Any) \str, Int \ch = 1 --> Str) {
    my \shft = abs(ch % str.chars);
    str.substr(shft) ~ str.substr(0, shft)
}
is rotate-it('Rakudo', 3), 'udoRak', 'coercion + sigilless params bind correctly';
is rotate-it('Rakudo'),    'akudoR', 'default sigilless param binds correctly';

# A `my \str` is lexically scoped to its block; outside it, the bare word
# resolves to the native `str` type again (the compiler must not leak the
# block-local sigilless binding past the block).
{
    my \str = "inner";
    #= just declares the shadow; nothing to assert here
}
is str.^name, 'str', 'block-local \str does not leak past its block';
