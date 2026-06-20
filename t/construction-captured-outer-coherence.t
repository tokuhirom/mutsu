use Test;

# Slice F coherence pin: a `submethod BUILD`/`TWEAK` run during `.new` can mutate
# a *captured-outer* caller lexical by name. Unlike a compiled method (whose
# `merge_method_env` records the write-through), the interpreter build phase
# bypasses that machinery, so the caller's slot was previously only reconciled by
# the blanket reverse `sync_locals_from_env` pull. These cases must stay coherent
# WITHOUT that pull (run the file with MUTSU_NO_REVERSE_SYNC=1 to verify).

plan 9;

# 1. BUILD mutates a captured-outer scalar counter.
{
    my $counter = 0;
    class B1 { submethod BUILD { $counter++ } }
    B1.new;
    is $counter, 1, 'BUILD captured-outer scalar increment is visible to caller';
}

# 2. TWEAK mutates a captured-outer string (no attributes).
{
    my $log = '';
    class T1 { submethod TWEAK { $log ~= 'T1' } }
    T1.new;
    is $log, 'T1', 'TWEAK captured-outer string append (no attrs) is visible';
}

# 3. TWEAK on an attribute-bearing class (native construction path).
{
    my $log = '';
    class T2 { has $.x; submethod TWEAK { $log ~= 'T2' } }
    T2.new;
    is $log, 'T2', 'TWEAK captured-outer write through the attribute native path';
}

# 4. Inheritance: parent + child BUILD both append, in order.
{
    my $calls = '';
    class P3 { submethod BUILD { $calls ~= 'P' } }
    class C3 is P3 { submethod BUILD { $calls ~= 'C' } }
    C3.new;
    is $calls, 'PC', 'inherited BUILD submethods both mutate the captured-outer var';
}

# 5. Inheritance with attributes: parent + child TWEAK, in order.
{
    my $tracker = '';
    class P4 { has $.x; submethod TWEAK { $tracker ~= "P4\n" } }
    class C4 is P4 { has $.y; submethod TWEAK { $tracker ~= "C4\n" } }
    C4.new(x => 1, y => 2);
    is $tracker, "P4\nC4\n", 'inherited TWEAK with attributes preserves order + captured-outer';
}

# 6. BUILD mutates a captured-outer array via push.
{
    my @seen;
    class B5 { submethod BUILD { @seen.push('built') } }
    B5.new;
    is @seen.join(','), 'built', 'BUILD captured-outer array push is visible';
}

# 7. Multiple constructions accumulate into the same captured-outer var.
{
    my $n = 0;
    class B6 { submethod TWEAK { $n++ } }
    B6.new; B6.new; B6.new;
    is $n, 3, 'repeated construction accumulates captured-outer mutations';
}

# 8. Construction assigned into a variable (`my $obj = Class.new`) still reconciles.
{
    my $hit = 0;
    class B7 { has $.v; submethod BUILD { $hit = 42 } }
    my $obj = B7.new;
    is $hit, 42, 'assigned-construction captured-outer write is visible';
}

# 9. The constructed instance itself is still correct (no collateral damage).
{
    my $side = '';
    class B8 { has $.v is rw; submethod TWEAK { $side = 'tweaked'; $!v = 99 } }
    my $o = B8.new;
    is $o.v ~ '|' ~ $side, '99|tweaked', 'attribute set in TWEAK and captured-outer both hold';
}
