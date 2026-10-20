use v6;
use Test;

plan 5;

# The type-check exception a store raises itself resumes at the next
# statement, as a `die` does (#11691). The container keeps what it had.
# Expected values are the `raku` output.

my @log;
{
    my Int $x = "a";
    @log.push: "after:" ~ $x.raku;
    CATCH { default { @log.push: .^name; .resume } }
}
is @log.join(' '), 'X::TypeCheck::Assignment after:Int', 'a declaration resumes with its type object';

@log = ();
{
    my Int $y = 5;
    $y = "b";
    @log.push: "after:" ~ $y;
    CATCH { default { @log.push: .^name; .resume } }
}
is @log.join(' '), 'X::TypeCheck::Assignment after:5', 'an assignment resumes with the old value';

@log = ();
for 1..2 {
    LEAVE @log.push: "l$_";
    my Int $z = "a";
    @log.push: "after";
    CATCH { default { @log.push: 'c'; .resume } }
}
is @log.join(' '), 'c after l1 c after l2', 'each loop iteration resumes and runs its LEAVE';

@log = ();
sub f {
    my Str $s = 42;
    @log.push: "in-f";
    'done'
}
{
    @log.push: f();
    CATCH { default { @log.push: 'c'; .resume } }
}
is @log.join(' '), 'c in-f done', 'a store in a called routine resumes in that routine';

@log = ();
{
    my Int $q = "a";
    @log.push: "unreached";
    CATCH { default { @log.push: 'handled' } }
}
is @log.join(' '), 'handled', 'a handler that does not resume still leaves the block';
