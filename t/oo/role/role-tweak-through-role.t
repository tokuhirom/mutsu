use Test;
plan 3;

my @log;
role R { submethod TWEAK { @log.push: "R.TWEAK" } }
role S does R { has $.s }
class C does S { }
C.new(:s<a>);
is @log.join(","), "R.TWEAK", "TWEAK of a role composed through another role runs once";

@log = ();
role S2 does R { has Str:D $.s is required }
class C2 does S2 { has Int:D $.n = 0 }
C2.new(:s<a>);
is @log.join(","), "R.TWEAK", "same with required attribute and defaulted class attribute";

@log = ();
class C3 does R { }
C3.new;
is @log.join(","), "R.TWEAK", "direct composition still runs once";
