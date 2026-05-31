use Test;
plan 5;

# .emit as a method on values inside supply blocks
{
    my $s = supply { "foo".emit; 42 .emit; 0.5.emit };
    my @got;
    $s.tap(-> $v { @got.push($v) });
    is @got[0], "foo", ".emit on Str works";
    is @got[1], 42, ".emit on Int works";
    is @got[2], 0.5, ".emit on Rat works";
}

# .emit with for loop (topic variable)
{
    my $s = supply { .emit for "a", "b", "c" };
    my @got;
    $s.tap(-> $v { @got.push($v) });
    is @got.join(","), "a,b,c", ".emit in for loop works";
}

# .emit inside react/whenever/supply
{
    my @got;
    react {
        whenever supply { .emit for 1, 2, 3 } {
            @got.push($_);
        }
    }
    is @got.join(","), "1,2,3", ".emit in react/whenever/supply works";
}
