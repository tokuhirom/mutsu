use v6;
use Test;

# Pin for deferral (nextsame/callsame) semantics in submethod dispatch.
# The slow-path dispatcher skips building the remaining-candidates list for
# single-candidate submethods (BUILD/TWEAK on every construction); these
# cases pin that multi-submethod deferral within one class still works and
# that a submethod never defers to an ancestor's submethod.

plan 4;

my @events;

# nextsame in a TWEAK submethod must NOT re-run the ancestor's TWEAK
# (submethods are not inherited); it terminates the submethod instead.
class A { submethod TWEAK { @events.push('A') } }
class B is A {
    submethod TWEAK { @events.push('B-before'); nextsame; @events.push('B-after') }
}

@events = ();
B.new;
is @events.join(','), 'A,B-before',
    'nextsame in TWEAK terminates without re-running ancestor TWEAK';

# nextsame in a multi submethod defers to the next same-class candidate.
class C {
    multi submethod greet(Int $x) { @events.push('Int'); nextsame; @events.push('Int-after') }
    multi submethod greet(Any $x) { @events.push('Any') }
}

@events = ();
C.new.greet(42);
is @events.join(','), 'Int,Any', 'nextsame defers to next multi submethod candidate';

@events = ();
C.new.greet('str');
is @events.join(','), 'Any', 'multi submethod dispatch picks the narrower candidate set';

# callsame in TWEAK likewise finds no ancestor candidate and resumes.
class D { submethod TWEAK { @events.push('D') } }
class E is D {
    submethod TWEAK { @events.push('E'); callsame; @events.push('E-done') }
}

@events = ();
E.new;
is @events.join(','), 'D,E,E-done',
    'callsame in TWEAK resumes without calling ancestor TWEAK';

done-testing;
