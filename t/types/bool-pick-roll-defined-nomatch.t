use Test;

# A defined Bool has only `Bool:U` candidates for `pick`/`roll`, and the
# hand-built NoMatch messages read rakudo's candidates from the generated table
# (#16736).

plan 8;

sub failure(&code) {
    code();
    CATCH { default { return ($_.^name, $_.message) } }
    ('none', '');
}

{
    my ($name, $msg) = failure { True.pick(2) };
    is $name, 'X::Multi::NoMatch', 'True.pick(2) is a NoMatch';
    is $msg, "Cannot resolve caller pick(Bool:D: Int:D); none of these signatures matches:\n    (Bool:U \$:: *%_)\n    (Bool:U \$:: \$n, *%_)",
        'with the Bool:U candidates';
}

{
    my ($name, $msg) = failure { False.roll };
    is $name, 'X::Multi::NoMatch', 'False.roll is a NoMatch';
    is $msg.lines[0], 'Cannot resolve caller roll(Bool:D: ); none of these signatures matches:',
        'with an empty argument profile';
}

ok Bool.pick ~~ Bool, 'the type object still picks';

{
    my ($name, $msg) = failure { (1, 2).grep(:k) };
    is $msg, "Cannot resolve caller grep(List:D: :k); none of these signatures matches:\n    (\$:: Bool:D \$t, *%_)\n    (\$:: Mu \$t, *%_)",
        'grep without a matcher lists the Any candidates';
}

{
    class Foo { }
    my ($name, $msg) = failure { Foo.new.Int };
    is $msg, "Cannot resolve caller Int(Foo:D: ); none of these signatures matches:\n    (Mu:U \\v:: *%_)",
        'a plain object .Int lists the Mu candidate';
}

{
    my @a;
    my ($name, $msg) = failure { @a[a => 1] };
    is $msg, "Cannot resolve caller Int(Pair:D: ); none of these signatures matches:\n    (Mu:U \\v:: *%_)",
        'a Pair index lists the Mu candidate';
}
