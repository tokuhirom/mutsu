use v6;
use Test;

# A `.map`/`.grep` callback that returns a FINITE lazy pipe (a `.grep`/`.map`
# chained on a `gather`) yields that pipe as an element of the result array.
# The result array's *static* readers (`.flat`, `for`, `value_to_list`) cannot
# run the VM to force a nested pipe, so before this fix they pulled the still
# empty pipe cache and produced `()`. Each element must be reified to a Seq.

plan 12;

# --- plain .map (topic $_) ---
{
    my @r = (1,).map({ gather { take 10; take 20 }.grep(* > 0) }).flat;
    is @r.elems, 2, 'flat over map-of-finite-grep-pipe reifies (elems)';
    is @r, [10, 20], 'flat over map-of-finite-grep-pipe reifies (values)';
}

# --- the element stays a SINGLE Seq (not flattened into the outer list) ---
{
    my @r = (1,).map({ gather { take 10; take 20 }.grep(* > 0) });
    is @r.elems, 1, 'map result keeps the pipe as one element';
    is @r[0].^name, 'Seq', 'the element is a Seq';
    is @r[0], (10, 20), 'the element reifies to its values';
}

# --- named param -> $r ---
{
    my @g = (1,).map(-> $r { gather { take 10; take 20 }.grep(* > 0) }).flat;
    is @g, [10, 20], 'named-param map over finite pipe reifies';
}

# --- multi-element source ---
{
    my @r = (1, 2).map({ gather { take $_ * 10; take $_ * 20 }.grep(* > 0) }).flat;
    is @r, [10, 20, 20, 40], 'multi-element map over finite pipe reifies';
}

# --- rw map (topic writeback) ---
{
    my @a = <a b>;
    my @r = @a.map({ gather { take $_; take $_ ~ "!" }.grep(*.chars > 0) }).flat;
    is @r, ['a', 'a!', 'b', 'b!'], 'rw map over finite pipe reifies';
}

# --- hyper batch map -> grep -> flat (the zef Repository.candidates shape) ---
{
    my @ids = (1,);
    my @g = @ids.hyper(:batch(1)).map(-> $r { gather { take 10; take 20 }.grep(* > 0) }).flat;
    is @g, [10, 20], 'hyper map over finite pipe reifies';
}

# --- for over the container of pipes ---
{
    my @seen;
    for (1,).map({ gather { take 5 }.grep(* > 0) }) -> $seq {
        @seen.push($seq.join(','));
    }
    is @seen, ['5'], 'for over container-of-pipes forces each element';
}

# --- an INFINITE pipe element must stay lazy (no hang, no eager force) ---
{
    my @r = (1,).map({ (1..Inf).map(* + 1) });
    is @r[0][^3], (2, 3, 4), 'infinite pipe element stays lazy (plain map)';
}
{
    my @r = (1,).hyper(:batch(1)).map(-> $r { (1..Inf).map(* + 1) });
    is @r[0][^3], (2, 3, 4), 'infinite pipe element stays lazy (hyper map)';
}
