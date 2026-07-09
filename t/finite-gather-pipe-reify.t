use v6;
use Test;

plan 14;

# A `.map`/`.grep` pipeline over a *finite* `gather` source must fully reify
# when materialized (`.List`/`.elems`/`for`/`.Array`), instead of returning an
# unforced `(...)`. An INFINITE pipeline (`(1..Inf).map(...)`) must stay lazy.

# --- finite gather + grep ---
{
    my $g = gather { take 1; take 2; take 3 };
    is $g.grep(* > 1).List, (2, 3), 'grep over finite gather reifies via .List';
    is $g.grep(* > 1).elems, 2, 'grep over finite gather reifies via .elems';
}

# --- finite gather + map ---
{
    is (gather { take 1; take 2; take 3 }).map(* + 10).List, (11, 12, 13),
        'map over finite gather reifies via .List';
    is (gather { take 1; take 2; take 3 }).map(* + 10).Array, [11, 12, 13],
        'map over finite gather reifies via .Array';
}

# --- for-loop over a finite gather pipe ---
{
    my @collected;
    for gather { take 1; take 2 }.grep(* > 0) -> $x { @collected.push($x) }
    is @collected, [1, 2], 'for loop over finite gather grep pipe iterates all';
}

# --- chained grep + map over a finite gather ---
{
    is (gather { take 1; take 2; take 3; take 4 }).grep(* %% 2).map(* * 10).List,
        (20, 40), 'chained grep+map over finite gather reifies';
}

# --- .join / .sum over a finite gather pipe ---
{
    is (gather { take 1; take 2; take 3 }).map(* + 1).join(','), '2,3,4',
        '.join over finite gather map pipe';
    is (gather { take 1; take 2; take 3 }).grep(* > 1).sum, 5,
        '.sum over finite gather grep pipe';
}

# --- INFINITE pipeline must remain lazy (no hang, slice works) ---
{
    is (1..Inf).map(* + 1).List[^3], (2, 3, 4),
        'infinite range map stays lazy (slice)';
    is (1..Inf).grep(* %% 2).head(3), (2, 4, 6),
        'infinite range grep stays lazy (head)';
    dies-ok { (1..Inf).map(* + 1).elems },
        'infinite range map .elems still raises (cannot be reified)';
}

# --- assigning a finite gather pipe to an array reifies ---
{
    my @a = gather { take 5; take 6 }.map(* + 1);
    is @a, [6, 7], 'finite gather pipe assigned to @ reifies';
    is @a.elems, 2, 'assigned array has all elements';
}

# --- empty finite gather pipe ---
{
    is (gather { }).grep(* > 0).List, (), 'empty gather grep pipe is empty';
}
