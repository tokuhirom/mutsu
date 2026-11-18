# Reaches RequireInRoutineLexB through a lazy `require` inside a routine, in
# a `Lock.protect` block, as Monad::Maybe reaches Monad::Either (#18017).
# Its own file-scope lexicals share the names of B's.
my $lock = Lock.new;
my $cache = Map.new;
my $B;
sub b-type() {
    $lock.protect({ $B //= (require ::('RequireInRoutineLexB')) });
    $B
}
class RequireInRoutineLexA {
    method b { b-type() }
    method own-cache { $cache }
}
