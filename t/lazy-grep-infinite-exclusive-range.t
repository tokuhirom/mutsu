use Test;

# grep/map over an *exclusive-end* infinite range (`^Inf`, `0..^Inf`) must be
# truly lazy (apply the matcher), like `0..Inf` / `1..Inf`. And a `last`/`next`
# inside the callback must be honored, so an `.eager` that the callback
# terminates with `last` returns a finite result instead of failing.

plan 14;

# --- ^Inf grep applies the matcher lazily ---
is (^Inf).grep(* %% 5)[^3].join(','), '0,5,10', '^Inf grep filters lazily';
is (0..^Inf).grep(* %% 5)[^3].join(','), '0,5,10', '0..^Inf grep filters lazily';
is (^Inf).grep(* > 3)[^3].join(','), '4,5,6', '^Inf grep with a threshold';
is (^Inf).grep(* %% 5).head(4).join(','), '0,5,10,15', '.head over a ^Inf grep';
is (^Inf).grep(* %% 5)[2], 10, 'index into a ^Inf grep';
ok (^Inf).grep(*.is-prime).is-lazy, '^Inf grep is lazy';
is (^Inf).grep(*.is-prime).head(5).join(','), '2,3,5,7,11', 'primes from ^Inf';

# --- map over ^Inf stays lazy ---
is (^Inf).map(* * 2)[^3].join(','), '0,2,4', '^Inf map is lazy';

# --- last inside the callback terminates an otherwise-infinite grep ---
is (^Inf).grep({ last if $_ > 5; True }).eager.join, '012345',
    'last in grep over ^Inf terminates .eager';
is (1..Inf).grep({ last if $_ > 5; True }).eager.join, '12345',
    'last in grep over 1..Inf terminates .eager';
is (0..Inf).grep({ last if $_ > 5; True }).eager.join, '012345',
    'last in grep over 0..Inf terminates .eager';

# --- next inside the callback skips elements ---
is (^Inf).grep({ next if $_ %% 2; $_ > 0 }).head(3).join(','), '1,3,5',
    'next in grep over ^Inf skips elements';

# --- last in a map over ^Inf terminates too ---
is (^Inf).map({ last if $_ > 3; $_ * 10 }).eager.join(','), '0,10,20,30',
    'last in map over ^Inf terminates .eager';

# --- finite ranges are unaffected ---
is (^10).grep(* %% 5).join(','), '0,5', 'finite range grep unchanged';
