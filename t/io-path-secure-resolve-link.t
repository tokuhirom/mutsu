use Test;

plan 12;

# .resolve(:completely) fails (returns a Failure) with X::IO::Resolve
{
    my $p = "/this/does/not/exist/at/all/foo/bar".IO;
    my $r = $p.resolve(:completely);
    isa-ok $r, Failure, '.resolve(:completely) returns a Failure on unresolvable path';
    throws-like { $r.sink }, X::IO::Resolve, '.resolve(:completely) Failure throws X::IO::Resolve';
}

# a resolved path's CWD is the SPEC dir separator
is "/tmp/../etc".IO.resolve.CWD, '/', '.resolve sets CWD to dir-sep';

# .child(:secure)
{
    my $parent = $*TMPDIR.child("io-secure-{$*PID}");
    $parent.mkdir;
    LEAVE { try $parent.child('foo').rmdir; try $parent.rmdir; }

    # escaping the parent fails with X::IO::NotAChild
    my $esc = $parent.child('../foo', :secure);
    isa-ok $esc, Failure, '.child(../foo, :secure) is a Failure';
    throws-like { $esc.sink }, X::IO::NotAChild, 'escaping child throws X::IO::NotAChild';

    # non-resolving intermediate fails with X::IO::Resolve
    my $nr = $parent.child('nope/bar', :secure);
    throws-like { $nr.sink }, X::IO::Resolve, 'non-resolving child throws X::IO::Resolve';

    # a valid (non-existent leaf) child resolves fine
    is-path $parent.child('foo', :secure), $parent.child('foo'),
        'valid non-existent child passes :secure';
    $parent.child('foo').mkdir;
    is-path $parent.child('foo/../bar', :secure), $parent.child('bar'),
        'child with ../ that stays inside passes :secure';
}

# .link creates a hard link and fails with X::IO::Link
{
    my $target = $*TMPDIR.child("io-link-tgt-{$*PID}");
    my $link   = $*TMPDIR.child("io-link-lnk-{$*PID}");
    LEAVE { try $target.unlink; try $link.unlink; }

    # target does not exist yet -> Failure
    my $r = link($target, $link);
    throws-like { $r.sink }, X::IO::Link, 'link to non-existent target fails X::IO::Link';

    $target.spurt: 'hello';
    is-deeply $target.link($link), True, '.link creates a hard link';
    is-deeply $link.slurp, 'hello', 'hard link reads target content';
}

# .IO on an IO::Path subclass type object returns the type object itself
is-deeply (IO::Path::Unix === IO::Path::Unix.IO), True, '.IO on :U type object is identity';
