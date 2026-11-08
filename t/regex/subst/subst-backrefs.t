use Test;

plan 9;

# Basic $0 in substitution replacement
{
    my $x = 'a1';
    $x ~~ s/(\d+)/<$0>/;
    is $x, 'a<1>', '$0 in substitution replacement';
}

# $0 with longer match
{
    my $str = 'ACCB';
    $str ~~ s/A (<-[B]>*) B/$0/;
    is $str, 'CC', '$0 replaces A...B with captured group';
}

# $0 and $1 in substitution
{
    my $x = '11 22 33';
    $x ~~ s/\s(\S+)\s(\S+)/-$0-$1/;
    is $x, '11-22-33', '$0 and $1 in substitution';
}

# $0 outside for loop
{
    my $x = 'a1';
    $x ~~ s/(\d+)/<$0>/;
    is $x, 'a<1>', 'substitution with backreference outside loop';
}

# $0 inside for loop
{
    for 1 {
        my $x = 'a1';
        $x ~~ s/(\d+)/<$0>/;
        is $x, 'a<1>', 'substitution with backreference inside loop';
    }
}

# Non-destructive S/// with $0
{
    my $x = 'hello123world';
    # The method's replacement is the string the caller built: a single-quoted
    # `$0` is a literal (Rakudo), only `s///` and closures see the captures.
    my $result = $x.subst(/(\d+)/, 'num:$0');
    is $result, 'hellonum:$0world', '.subst with a literal $0 replacement';
    is $x.subst(/(\d+)/, { "num:$0" }), 'hellonum:123world', '.subst closure with $0';
}

# Global substitution with $0
{
    my $x = 'a1b2c3';
    $x ~~ s:g/(\d+)/<$0>/;
    is $x, 'a<1>b<2>c<3>', 'global substitution with $0';
}

# No captures - $0 should produce empty string
{
    my $x = 'hello';
    $x ~~ s/hello/bye/;
    is $x, 'bye', 'substitution without captures works';
}
