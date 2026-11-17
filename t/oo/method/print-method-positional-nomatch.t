use Test;

# `Mu.print`/`Mu.note` take no positional: a call with one on an object that
# does not override them fails their dispatch with X::Multi::NoMatch, not a
# method lookup (#17857). A Failure held in a variable must not explode.

plan 9;

sub failure(&code) {
    code();
    CATCH { default { return ($_.^name, $_.message) } }
    ('none', '');
}

{
    my ($name, $msg) = failure { 1.print(2) };
    is $name, 'X::Multi::NoMatch', 'Int.print(2) is a NoMatch';
    is $msg, "Cannot resolve caller print(Int:D: Int:D); none of these signatures matches:\n    (Mu \$:: *%_)",
        'with the Mu candidate';
}

{
    my ($name, $msg) = failure { 1.note(2) };
    is $msg.lines[0], 'Cannot resolve caller note(Int:D: Int:D); none of these signatures matches:', 'note too';
}

{
    my ($name, $msg) = failure { "a".print("b", "c") };
    is $msg.lines[0], 'Cannot resolve caller print(Str:D: Str:D, Str:D); none of these signatures matches:',
        'a Str invocant with two positionals';
}

{
    class Foo { }
    my ($name, $msg) = failure { Foo.new.print(1) };
    is $msg.lines[0], 'Cannot resolve caller print(Foo:D: Int:D); none of these signatures matches:',
        'a user class without its own print';
}

{
    my ($name, $msg) = failure { Int.print(1) };
    is $msg.lines[0], 'Cannot resolve caller print(Int:U: Int:D); none of these signatures matches:',
        'a type object';
}

{
    my $f = open("/nonexistent/dir/file");
    my ($name, $msg) = failure { $f.print(1) };
    is $name, 'X::Multi::NoMatch', 'a Failure in a variable does not explode';
    ok $msg.contains('Cannot resolve caller print(Failure:U: Int:D); none of these signatures matches:'),
        'and is named Failure:U';
}

{
    class Bar { method print($x) { "bar-$x" } }
    is Bar.new.print(1), 'bar-1', 'a class with its own print still answers';
}
