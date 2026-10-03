use Test;

plan 18;

{
    my @source = 1, 2;
    my $holder = @source;
    my &write = { $holder = 5 };
    write();
    is @source.raku, '[1, 2]', 'closure assignment leaves the shared array intact';
    is $holder, 5, 'closure assignment updates its scalar holder';
}

{
    my @source = 1, 2;
    my $holder = @source;
    sub write($target is rw) { $target = 5 }
    write($holder);
    is @source.raku, '[1, 2]', 'rw assignment leaves the shared array intact';
    is $holder, 5, 'rw assignment updates its scalar holder';
}

{
    my @source = 1, 2;
    my $holder = @source;
    given $holder { $_ = 5 }
    is @source.raku, '[1, 2]', 'topic alias assignment leaves the shared array intact';
}

{
    my @source = 1, 2;
    my $holder = @source;
    for $holder <-> $target { $target = 5 }
    is @source.raku, '[1, 2]', 'rw loop alias assignment leaves the shared array intact';
}

{
    my @source = 1, 2;
    my $holder;
    my $alias := $holder;
    $alias = @source;
    is $holder.raku, '$[1, 2]', 'a bound target retains the shared value';
    @source.push: 3;
    is $holder.raku, '$[1, 2, 3]', 'the bound target tracks later source mutation';
}

{
    my @source = 1, 2;
    my $first = @source;
    my $second = $first;
    $first = 5;
    @source.push: 3;
    is $first, 5, 'a chained source can be reassigned independently';
    is $second.raku, '$[1, 2, 3]', 'a chained target retains the aggregate share';
}

{
    my $first = [1, 2];
    my $second = $first;
    $first = 5;
    is $first, 5, 'a direct scalar aggregate can be reassigned independently';
    is $second.raku, '$[1, 2]', 'its chained share retains the original aggregate';
}

{
    my @source = 1, 2;
    my $holder;
    my $bound;
    my &write = { $holder = @source };
    $holder := $bound;
    write();
    is $holder.raku, '$[1, 2]', 'a captured rebound holder receives the share';
    is $bound.raku, '$[1, 2]', 'the rebound container sees the share';
}

{
    my @source = 1, 2;
    my $holder = @source;
    throws-like { $holder++ }, X::Method::NotFound,
        'incrementing a scalar holding an array calls the missing succ method';
    is @source.raku, '[1, 2]', 'failed increment leaves the source intact';
    is $holder.raku, '$[1, 2]', 'failed increment leaves the holder intact';
}

{
    sub call(&body) { body() }
    call {
        my %source = a => 1;
        sub write($target is rw) { $target = %source }
        my $holder;
        write($holder);
        is $holder.raku, '${:a(1)}', 'a captured rw target retains a hash share';
    }
}
