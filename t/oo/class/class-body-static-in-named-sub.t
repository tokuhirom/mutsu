use v6;
use Test;

# A class's body `my` static (`class C { my $x = ...; method m { $x } }`) is
# already reachable from a METHOD (see the sibling `class-body-static-in-sub.t`).
# A package `sub` (plain or `our sub`) declared in the same class body must see
# the same, fully-initialized static rather than an empty/undefined snapshot
# taken before the class body's own initializer ran (#8869).

plan 6;

# 1-2. An `our sub` reading a class-body `my` array static, called from
# outside the class after it is fully composed.
class Foo {
    my @abbrs = < Muh Saf R.A R.T J.A J.T
                  Raj Sha Ram Shw Qid Hij >;

    our sub month-abbr(Int:D $month) {
        return @abbrs[$month - 1];
    }
}
is Foo::month-abbr(6), 'J.T', 'our sub reads a class-body my array static';
is Foo::month-abbr(1), 'Muh', 'our sub reads a different element of the same static';

# 3. An `our sub` reading a class-body `my` scalar static, mutating it across
# calls (shared, like the method-static-in-sub counterpart).
class Bar {
    my $count = 41;
    our sub bump() {
        $count++;
        return $count;
    }
}
is Bar::bump(), 42, 'our sub reads and mutates a class-body my scalar static';

# 4. An `our sub` reading a class-body `my` hash static.
class Baz {
    my %h = a => 1, b => 2;
    our sub get-h() {
        return %h;
    }
}
is-deeply Baz::get-h(), { a => 1, b => 2 }, 'our sub reads a class-body my hash static';

# 5-6. A plain (non-`our`) `sub` reads the same static too, called from a
# method in the same class (a plain sub is lexically scoped, not reachable via
# `Class::name` from outside).
class Qux {
    my @arr = (10, 20, 30);
    sub get-arr() {
        return @arr;
    }
    method call-get-arr {
        return get-arr();
    }
}
is-deeply Qux.new.call-get-arr, [10, 20, 30], 'plain sub reads a class-body my array static';

class Quux {
    my $n = 7;
    sub get-n() {
        return $n;
    }
    method call-get-n {
        return get-n();
    }
}
is Quux.new.call-get-n, 7, 'plain sub reads a class-body my scalar static';
