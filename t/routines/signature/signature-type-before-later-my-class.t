use Test;

# A signature's type names resolve where the signature is written: a
# `my class DateTime is DateTime { }` declared LATER in the same scope must not
# rebind the name for a routine above it (DateTime::strftime, lizmat's
# refining-subclass idiom).

plan 6;

# Same file: the routine keeps the core type, the later subclass is a DateTime.
{
    my sub take(DateTime:D $dt --> Str:D) { "took " ~ $dt.year }
    my sub take-plain(DateTime $dt) { "plain " ~ $dt.year }
    my class DateTime is DateTime { }
    is take(CORE::DateTime.new(2025, 1, 5, 4, 7, 42)), 'took 2025', 'core DateTime binds';
    is take(DateTime.new(2025, 1, 5, 4, 7, 42)), 'took 2025', 'the later subclass binds too';
    is take-plain(CORE::DateTime.new(2025, 1, 5, 4, 7, 42)), 'plain 2025', 'unsmiled constraint';
    throws-like { take(3) }, X::TypeCheck::Binding::Parameter,
        message => / 'expected DateTime' .* 'but got Int' /, 'the error names the type, not CORE::';
}

# From a module file: the routine is called from a scope that never saw the class.
{
    my $dir = $*TMPDIR.add("mutsu-shadow-" ~ $*PID ~ "-" ~ (^1000000).pick);
    LEAVE { try { .unlink for $dir.dir; $dir.rmdir } }
    $dir.mkdir;
    $dir.add('Shadow').mkdir;
    $dir.add('Shadow/Dt.rakumod').spurt: q:to/MOD/;
        my sub take(DateTime:D $dt, Str:D $f --> Str:D) is export(:DEFAULT, :refine) { "took " ~ $dt.year }
        my class DateTime is DateTime { }
        MOD
    my $out = EVAL "use lib '$dir'; use Shadow::Dt; take(DateTime.new(2025,1,5,4,7,42,:timezone(3600)), 'x')";
    is $out, 'took 2025', 'module routine above its refining class takes a core DateTime';
}

# A routine declared AFTER the class sees the class.
{
    my class Date is Date { method marker { 'refined' } }
    my sub after(Date:D $d) { $d.marker }
    is after(Date.new(2025, 1, 5)), 'refined', 'a later routine sees the lexical class';
}
