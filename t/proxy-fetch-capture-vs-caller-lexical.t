use Test;

plan 9;

# A `Proxy` FETCH/STORE body is invoked with caller-priority env inputs so that
# it can see the CURRENT value of a captured lexical its STORE twin mutates.
# That must not let an *unrelated* caller lexical that merely shares a name
# shadow a capture the closure lexically owns.

our sub mkproxy($libname) is rw {
    Proxy.new(FETCH => -> $ { "saw:" ~ ($libname // 'UNDEF') }, STORE => -> $, $ { })
}

sub no-collision() { mkproxy('INNER') }
sub my-collision() { my $libname = 'OUTER'; mkproxy('INNER') }
sub param-collision(Str $libname) { mkproxy('INNER') }

is no-collision(), 'saw:INNER', 'FETCH sees its capture when the caller has no such name';
is my-collision(), 'saw:INNER', 'a same-named caller `my` lexical does not shadow the capture';
is param-collision('PARAM'), 'saw:INNER',
    'a same-named caller parameter does not shadow the capture';

# The FETCH body's capture must not leak back out and clobber the caller's
# same-named lexical either -- the shape that made every probe after the first
# one in NativeLibs' `try-versions` read the previous candidate's library name.
sub probe-loop(Str $libname) {
    my @seen;
    for 0..2 -> $v {
        my $candidate = "$libname.so.$v";
        @seen.push(mkproxy($candidate));
        @seen.push($libname);
    }
    @seen
}
is probe-loop('lib').join(','),
    'saw:lib.so.0,lib,saw:lib.so.1,lib,saw:lib.so.2,lib',
    'a FETCH body leaves the caller\'s same-named lexical alone across iterations';

# ... and the same through a method, which is the DBIish/NativeLibs shape.
class Searcher {
    method probe(Str $libname) {
        my @seen;
        for 0..2 -> $v {
            @seen.push(mkproxy("$libname.so.$v"));
        }
        @seen
    }
}
is Searcher.probe('lib').join(','), 'saw:lib.so.0,saw:lib.so.1,saw:lib.so.2',
    'the same holds when the caller is a method';

# Freshness is still there for the case caller-priority exists for: a lexical
# the STORE side MUTATES is not lexically owned by the FETCH body, so the live
# value must keep winning over the (stale) captured snapshot.
{
    my $backing = 'abc';
    my $p := Proxy.new(FETCH => -> $ { $backing }, STORE => -> $, $v { $backing = $v });
    is $p, 'abc', 'Proxy FETCH reads the backing lexical';
    $p = 'xyz';
    is $p, 'xyz', 'Proxy FETCH sees a value its STORE twin just wrote';
    is $backing, 'xyz', 'the backing lexical really was written';
    $backing = 'direct';
    is $p, 'direct', 'Proxy FETCH sees a direct write to the backing lexical';
}
