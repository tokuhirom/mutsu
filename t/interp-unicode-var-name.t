use v6;
use Test;

# String interpolation must recognise Unicode letters in a variable name.
# Previously the interpolation identifier scanner used is_ascii_alphabetic,
# so `"$ν"` split into `$` + literal `ν` and never interpolated.

plan 9;

{
    my $ν = 42;
    is "$ν is here", "42 is here", 'Greek scalar var interpolates';
}

{
    my $café = 1;
    is "$café!", "1!", 'accented scalar var interpolates';
}

{
    my $λ = 5;
    is "value: $λ", "value: 5", 'lambda scalar var interpolates';
}

# Method-call interpolation on a Unicode-named var.
{
    my $ν = 42;
    is "$ν.Str()", "42", 'method call on unicode var interpolates';
}

# A superscript digit still terminates the name (postfix exponent boundary):
# "$x²" is variable $x followed by a literal "²".
{
    my $x = 3;
    is "$x²", "3²", 'superscript digit ends the interpolated var name';
    my $no = 5;
    is "$no² done", "5² done", 'name stops before superscript, keeps the rest';
}

# Unicode array / hash interpolation still works.
{
    my @αβ = 1, 2, 3;
    is "list: @αβ[]", "list: 1 2 3", 'unicode array var interpolates';
    my %λ = a => 1;
    is "%λ<a>", "1", 'unicode hash var interpolates';
}

# Plain ASCII interpolation is unaffected.
{
    my $name = "world";
    is "hello $name", "hello world", 'ascii interpolation unaffected';
}
