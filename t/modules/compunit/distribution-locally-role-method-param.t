use Test;

plan 2;

# `Distribution::Locally` is a core role; a role method may constrain a
# parameter with it (Pakku::Core's `method !stage-dist ( $stage,
# Distribution::Locally:D $dist, ... )`).
role R {
    method !stage ( $stage, Distribution::Locally:D $dist, Bool:D :$build! ) { "$stage" }
    method run ( $dist ) { self!stage( 'x', $dist, :build ) }
}
class C does R { }

lives-ok { C.new }, 'a role method constraining a parameter with Distribution::Locally composes';
dies-ok { C.new.run( 42 ) }, 'a non-distribution argument fails the constraint';
