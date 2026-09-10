use Test;

# `our %g := %h` binds a package (global) hash variable to another hash,
# sharing the same container. Element writes through either name must be
# visible through both (the bind is an alias, not a snapshot).
#
# Regression: `our %g := %h` previously died with
#   "Cannot assign to a readonly variable (%g)"
# because the bind marks the var readonly as a bind signal and the global
# store (`SetGlobal`) tripped the readonly check. (The `my %g := %h` local
# form already worked.)

plan 9;

# Basic global hash bind + read.
{
    our %h = (x => 1);
    our %g := %h;
    is %g<x>, 1, 'read through bound global hash';
    is %g.elems, 1, 'elems through bound global hash';
}

# Write through the bound name reaches the source.
{
    our %h2 = (x => 1);
    our %g2 := %h2;
    %g2<x> = 42;
    is %h2<x>, 42, 'write through bound name reaches source';
    is %g2<x>, 42, 'write through bound name visible on bound name';
}

# Write through the source reaches the bound name (bidirectional alias).
{
    our %h3 = (a => 1, b => 2);
    our %g3 := %h3;
    %h3<a> = 10;
    is %g3<a>, 10, 'write through source reaches bound name';
    is %g3.elems, 2, 'bound name sees source size';
}

# Adding a new key through the bound name is visible on the source.
{
    our %h4 = (a => 1);
    our %g4 := %h4;
    %g4<c> = 3;
    is %h4<c>, 3, 'new key through bound name visible on source';
    is %h4.elems, 2, 'source grows when bound name adds a key';
}

# The `my` local form keeps working (no regression).
{
    my %h5 = (x => 1);
    my %g5 := %h5;
    %g5<x> = 7;
    is %h5<x>, 7, 'local := bind still shares container';
}
