use Test;

# `Any.map`/`grep`/`head`/`first` read `self.iterator` on demand, so a class
# that supplies a never-ending `iterator` (`is-lazy`) must not be drained up
# front. Found with Math::Sequences (`class Integers is Range` with its own
# lazy `iterator`; `use Math::Sequences::Integer` hung). #15288

plan 7;

class Nat {
    method iterator {
        class :: does Iterator {
            has $.n is rw = 0;
            method is-lazy { True }
            method pull-one { $!n++ }
        }.new
    }
}

is-deeply Nat.new.map(* * 2).head(3).List, (0, 2, 4), '.map over an infinite own iterator is lazy';
is-deeply Nat.new.head(3).List, (0, 1, 2), '.head(n) pulls only n items';
is-deeply Nat.new.grep(* %% 2).head(3).List, (0, 2, 4), '.grep over an infinite own iterator is lazy';
is Nat.new.first, 0, '.first pulls one item';
is-deeply Nat.new.map(* + 1).grep(* %% 3).head(2).List, (3, 6), 'a map/grep chain stays lazy';

class Three {
    method iterator {
        class :: does Iterator {
            has $.n is rw = 0;
            method pull-one { $!n < 3 ?? $!n++ !! IterationEnd }
        }.new
    }
}

is-deeply Three.new.map(* * 10).List, (0, 10, 20), 'a finite own iterator still maps fully';
is Three.new.sum, 3, 'a finite own iterator still sums';
