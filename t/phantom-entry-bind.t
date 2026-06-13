# Phantom-entry: a `:=` bind to a missing hash key materializes into a shared
# `ContainerRef` cell on the first write through the bound variable, so the
# bound var and the hash entry alias bidirectionally afterwards. Before the
# fix, materialization stored a plain value and the alias was lost (a later
# cross-write was not observed).
use Test;

plan 12;

# --- single missing key (terminal) ---
{
    my %m;
    my $e := %m<solo>;
    nok %m<solo>:exists, 'single: key does not exist before write';
    $e = 9;
    is %m<solo>, 9, 'single: write through bound var reaches the hash';
    ok %m<solo>:exists, 'single: key exists after write';
    # bidirectional after materialization
    %m<solo> = 10;
    is $e, 10, 'single: bound var sees a later write through the hash';
    $e = 11;
    is %m<solo>, 11, 'single: hash sees a later write through the bound var';
}

# --- missing nested key ---
{
    my %k;
    my $d := %k<p><q>;
    nok %k<p>:exists, 'nested: intermediate does not exist before write';
    $d = 1;
    is %k<p><q>, 1, 'nested: write through bound var materializes the path';
    ok %k<p>:exists, 'nested: intermediate exists after write';
    # bidirectional after materialization (the case-C regression)
    %k<p><q> = 2;
    is $d, 2, 'nested: bound var sees a later write through the hash';
    $d = 3;
    is %k<p><q>, 3, 'nested: hash sees a later write through the bound var';
}

# --- bind, then independent write through the hash path stays unaliased ---
# (Rakudo: a deferred missing-key bind only connects when written THROUGH the
# bound var; an independent hash write before that does not retroactively bind.)
{
    my %g;
    my $c := %g<x><y>;
    %g<x><y> = 42;
    ok !$c.defined, 'pre-write hash path write does not retro-bind the var';
}

# --- read before write does not autovivify ---
{
    my %h;
    my $b := %h<a><b>;
    my $ignore = $b;
    nok %h<a>:exists, 'read of bound var does not autovivify the path';
}
