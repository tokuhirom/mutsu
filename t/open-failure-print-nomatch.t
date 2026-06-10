use Test;

# open() on an unopenable path returns a Failure (rather than dying
# immediately); the Failure throws when sunk/used.
{
    my $r = open("/a/b/c/d/bogus", :w);
    isa-ok $r, Failure, "open() of a bad path returns a Failure";
    nok $r.defined, "the returned Failure is undefined";
}

# Calling print/say/put/note with a *positional* argument on the Failure
# resolves to the default (Mu: *%_) candidate, which takes no positional, so
# it throws X::Multi::NoMatch (it must NOT explode the Failure).
# https://github.com/rakudo/rakudo/issues/2492
{
    throws-like { open("/a/b/c/d/bogus", :w).print: 42 },
        X::Multi::NoMatch,
        capture => { so $_[0]; $_[0] ~~ Failure },
        "print(positional) on a Failure throws X::Multi::NoMatch";

    throws-like { open("/a/b/c/d/bogus", :w).say: 42 },
        X::Multi::NoMatch,
        "say(positional) on a Failure throws X::Multi::NoMatch";

    throws-like { open("/a/b/c/d/bogus", :w).put: 42 },
        X::Multi::NoMatch,
        "put(positional) on a Failure throws X::Multi::NoMatch";
}

# Opening a directory also yields a Failure (not a hard die).
{
    my $r = open("t");
    isa-ok $r, Failure, "open() of a directory returns a Failure";
}

# A NUL byte in the path is still a hard throw (X::IO::Null), not a Failure.
{
    my $nul = "foo\0bar";
    dies-ok { open($nul) }, "open() with a NUL byte in the path dies";
}

done-testing;
