use Test;

# A named sub in a `package`/`module` block closes over the block's `my`
# lexicals AND the package's `our` variables. Mutations made through one
# by-name call must persist and be visible to later by-name calls (the
# declaring package is restored on EVERY dispatch path, not just the first
# on-the-fly compile).

plan 18;

# --- read-only `my` lexical, called more than once -------------------------
{
    package P1 { my $X = "v"; our sub show() { $X } }
    is P1::show(), "v", "read-only my lexical, 1st call";
    is P1::show(), "v", "read-only my lexical, 2nd call (was Nil)";
}

# --- module behaves like package -------------------------------------------
{
    module M1 { my $X = "m"; our sub show() { $X } }
    is M1::show(), "m", "module my lexical, 1st call";
    is M1::show(), "m", "module my lexical, 2nd call";
}

# --- `my` lexical mutated by `++` accumulates across calls ------------------
{
    package P2 { my $X = 1; our sub inc() { $X++ }; our sub get() { $X } }
    P2::inc(); P2::inc();
    is P2::get(), 3, "my lexical ++ accumulates across by-name calls";
}

# --- compound assign `$X = $X + n` accumulates -----------------------------
{
    package P3 { my $X = 0; our sub bump() { $X = $X + 10 }; our sub get() { $X } }
    P3::bump(); P3::bump(); P3::bump();
    is P3::get(), 30, "my lexical compound-assign accumulates";
}

# --- nested unqualified call mutating the shared lexical --------------------
{
    package P4 {
        my $X = 0;
        our sub inc() { $X++ }
        our sub run() { inc(); inc(); inc(); $X }
    }
    is P4::run(), 3, "nested unqualified inc, 1st run";
    is P4::run(), 6, "nested unqualified inc accumulates into 2nd run";
}

# --- string concat through a parameterised sub -----------------------------
{
    package P5 {
        my $log = "";
        our sub add($s) { $log ~= $s }
        our sub dump() { $log }
    }
    P5::add("a"); P5::add("b"); P5::add("c");
    is P5::dump(), "abc", "concat into my lexical via param sub";
}

# --- pre-increment / pre-decrement -----------------------------------------
{
    package P6 { my $n = 5; our sub up() { ++$n }; our sub down() { --$n }; our sub get() { $n } }
    P6::up(); P6::up(); P6::down();
    is P6::get(), 6, "pre-inc/pre-dec on my lexical";
}

# --- `our` package variable, read repeatedly -------------------------------
{
    package P7 { our $X = 10; our sub show() { $X } }
    is P7::show(), 10, "read-only our var, 1st call";
    is P7::show(), 10, "read-only our var, 2nd call (was Nil)";
}

# --- `our` package variable mutated across calls ---------------------------
{
    package P8 { our $X = 1; our sub inc() { $X = $X + 1 }; our sub get() { $X } }
    P8::inc(); P8::inc();
    is P8::get(), 3, "our var mutated accumulates across calls";
}

# --- `our` var via `++` ----------------------------------------------------
{
    package P9 { our $X = 0; our sub inc() { $X++ }; our sub get() { $X } }
    P9::inc(); P9::inc(); P9::inc();
    is P9::get(), 3, "our var ++ accumulates across calls";
}

# --- array lexical mutated in place ----------------------------------------
{
    package P10 {
        my @a;
        our sub add($x) { @a.push($x) }
        our sub all() { @a.join(",") }
    }
    P10::add(1); P10::add(2); P10::add(3);
    is P10::all(), "1,2,3", "my array push accumulates across calls";
}

# --- multi-level package name ----------------------------------------------
{
    package A::B { my $X = 5; our sub bump() { $X++ }; our sub get() { $X } }
    A::B::bump(); A::B::bump();
    is A::B::get(), 7, "multi-level package name shares lexical";
}

# --- two packages with same lexical name stay isolated ---------------------
{
    package Q1 { my $X = 100; our sub inc() { $X++ }; our sub get() { $X } }
    package Q2 { my $X = 200; our sub inc() { $X++ }; our sub get() { $X } }
    Q1::inc(); Q2::inc(); Q2::inc();
    is Q1::get(), 101, "same-named lexical isolated per package (Q1)";
    is Q2::get(), 202, "same-named lexical isolated per package (Q2)";
}
