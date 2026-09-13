use Test;

plan 2;

# Class-body lexicals are persisted in the class package store after the body
# exits.  A compound assignment expression must write that store back, or a
# later call observes the stale declaration-time value.
class StaticCompoundAssignment {
    my $cached;

    method get() {
        $cached //= 42;
        $cached
    }
}

is StaticCompoundAssignment.new.get, 42, 'compound assignment initializes a class static';
is StaticCompoundAssignment.new.get, 42, 'the initialized class static persists';
