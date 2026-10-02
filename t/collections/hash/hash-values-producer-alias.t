use Test;

# `for %h.values` binds the element cells the container-aware `.values`
# producer hands out; the loop itself does no per-iteration key lookup
# (#10951). These pin the aliasing edges that the removed per-iteration
# `HashValue` re-lookup used to decide.
plan 8;

{
    my %h = a => 1, b => 2, c => 3;
    for %h.values { $_ *= 10 }
    for %h.values { $_ += 1 }
    is %h.sort».kv.flat.join(","), "a,11,b,21,c,31",
        'a second loop over already-promoted cells still aliases';
}

{
    my %h = a => 1, b => 2;
    for %h.values { %h = x => 7; $_ = 99 }
    is %h.sort».kv.flat.join(","), "x,7",
        'reassigning the hash in the body leaves the new contents alone';
}

{
    my %h = a => 1, b => 2;
    for %h.values { %h<a>:delete; $_ = 50 }
    nok %h<a>:exists, 'assigning a deleted element\'s alias does not revive the key';
}

{
    my Int %h = a => 1;
    throws-like { for %h.values { $_ = "s" } }, X::TypeCheck::Assignment,
        message => /'%h'/, 'a typed hash names itself in the topic type-check error';
}

{
    my %h = a => 1, b => 2, c => 3, d => 4;
    is %h.values.join(","), %h.keys.map({ %h{$_} }).join(","),
        '.values follows .keys order';
    is %h.kv.map(-> $k, $v { "$k=$v" }).join(","),
        %h.keys.map({ "$_=%h{$_}" }).join(","), '.kv follows .keys order';
    is %h.pairs.map(*.key).join(","), %h.keys.join(","), '.pairs follows .keys order';
}

{
    my %h = a => 1, b => 2;
    my $sum = 0;
    $sum += $_ for %h.values;
    is %h.sort».kv.flat.join(","), "a,1,b,2", 'a read-only sum leaves the hash unchanged';
}
