use Test;

# PDF::COS::Tie::Hash: `method AT-KEY($key, :$check) is rw { my $val := callsame; ... }`
# in a role composed into an `is Hash` class, read through an instance that
# got another role mixed in (`$dict but Info`, PDF's `.^mixin`). The mixin
# dropped the instance's storage as the base candidate of `callsame`, so the
# call answered Nil.

plan 5;

role Info { }
role TH { method AT-KEY($key, :$check) is rw { callsame } }
class Dict is Hash does TH { }

my $d = Dict.new;
$d<a> = "abc";
my $m = $d but Info;
is $m<a>, 'abc', 'callsame from a role AT-KEY reaches the storage of a mixed-in instance';
is $m.AT-KEY("a", :check), 'abc', 'also when the call names its adverb';
nok $m<missing>.defined, 'a missing key stays undefined';

class Own is Hash {
    method AT-KEY($key) is rw { my $r := callsame; $r.VAR.^name }
}
my $o = Own.new;
$o<k> = "v";
is $o<k>, 'Scalar', 'an own AT-KEY gets the element container from callsame';
is ($o but Info)<k>, 'Scalar', 'and so does the same object with a role mixed in';
