use Test;

plan 3;

{
    my $s = supply {
        emit 42;
    }

    my @values;
    $s.tap(-> $v { @values.push($v) });
    is-deeply @values, [42], 'supply block emits values';
}

{
    my $s = supply {
        emit 1;
        done;
        emit 2;
    }

    my @values;
    $s.tap(-> $v { @values.push($v) });
    is-deeply @values, [1], 'done stops further execution in supply block';
}

{
    my $s = supply {
        emit Mu;
    }

    my $seen = False;
    $s.tap: -> \val { $seen = val =:= Mu };
    ok $seen, 'sigilless pointy parameter works in tap callback';
}
