use Test;

# The output-capture idiom binds `$*OUT` (or `$*ERR`) to an object whose
# `print` method appends to a captured outer lexical, then reads that lexical
# after some `say`/`print`/`put` calls. Successive writes must ACCUMULATE — the
# internal dispatch to the handle's `print` had no surrounding CallMethod op to
# drain the captured-lexical writeback, so earlier writes were lost and only the
# last survived. (Same-frame capture; a capture across a passed code block is a
# deeper multi-frame case, still open.)

plan 5;

# --- say accumulates through a custom $*OUT ---
{
    my $out = "";
    {
        my $*OUT = class { method print(*@a) { $out ~= @a.join } }
        say "a";
        say "b";
        say "c";
    }
    is $out, "a\nb\nc\n", 'say accumulates through a custom $*OUT.print';
}

# --- print (no newline) accumulates ---
{
    my $out = "";
    {
        my $*OUT = class { method print(*@a) { $out ~= @a.join } }
        print "x";
        print "y";
    }
    is $out, "xy", 'print accumulates through a custom $*OUT.print';
}

# --- put accumulates ---
{
    my $out = "";
    {
        my $*OUT = class { method print(*@a) { $out ~= @a.join } }
        put "1";
        put "2";
    }
    is $out, "1\n2\n", 'put accumulates through a custom $*OUT.print';
}

# --- note accumulates through a custom $*ERR ---
{
    my $err = "";
    {
        my $*ERR = class { method print(*@a) { $err ~= @a.join } }
        note "e1";
        note "e2";
    }
    is $err, "e1\ne2\n", 'note accumulates through a custom $*ERR.print';
}

# --- capture inside a sub (same frame as the say calls) accumulates ---
{
    sub cap() {
        my $o = "";
        my $*OUT = class { method print(*@a) { $o ~= @a.join } }
        say "p";
        say "q";
        return $o;
    }
    is cap(), "p\nq\n", 'capture within a sub accumulates';
}
