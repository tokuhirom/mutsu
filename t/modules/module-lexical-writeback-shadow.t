use Test;
use lib 't/lib';
use UnitFileLexical;

plan 7;

{
    my $secret = "caller";
    poke("first");
    is $secret, "caller", "positional module call leaves the caller shadow intact";
    is peek(), "first", "positional module call updates the module lexical";
    poke("second");
    is $secret, "caller", "a second positional call leaves the caller shadow intact";
    is peek(), "second", "the module lexical keeps the second write";
    reset-secret();
    is $secret, "caller", "zero-argument module call leaves the caller shadow intact";
    is peek(), "reset", "zero-argument module call updates the module lexical";
}

is peek(), "reset", "module lexical survives the caller block";
