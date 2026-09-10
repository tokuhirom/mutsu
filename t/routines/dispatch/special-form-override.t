use Test;

plan 14;

# Defining a sub for a reserved special-form operator — one handled directly by
# the compiler grammar — throws X::Syntax::Extension::SpecialForm at compile time.
# The reserved set: infix:<=>, infix:<:=>, infix:<::=>, infix:<~~>, prefix:<|>.

sub special-form-throws($code, $category, $opname, $desc) {
    my $err;
    try { EVAL $code; CATCH { default { $err = $_ } } }
    isa-ok $err, X::Syntax::Extension::SpecialForm, $desc;
    is $err.category, $category, "$desc: category";
    is $err.opname,   $opname,   "$desc: opname";
}

special-form-throws 'multi sub infix:<=>(\a, \b) { }',  'infix',  '=',  'infix:<=>';
special-form-throws 'multi sub infix:<:=>(\a, \b) { }', 'infix',  ':=', 'infix:<:=>';
special-form-throws 'multi sub infix:<~~>(\a, \b) { }', 'infix',  '~~', 'infix:<~~>';
special-form-throws 'multi sub prefix:<|> (\a) { }',    'prefix', '|',  'prefix:<|>';

# Normal operator definitions are still allowed and callable.
{
    sub infix:<plus>($a, $b) { $a + $b }
    is 2 plus 3, 5, 'a normal infix operator definition still works';
}
{
    sub prefix:<§>($x) { $x * 2 }
    is §5, 10, 'a normal prefix operator definition still works';
}
