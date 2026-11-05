use Test;

# A `hyper for` / `race for` body runs on worker threads. The outer `@`/`%`
# lexicals it names are reached through their declaration-site cells
# (ADR-16595 rule 2), and a routine's own same-named lexical stays separate.

plan 10;

sub own-r() {
    my @r;
    @r.push(7);
    @r.push(8);
    @r.elems;
}

my $lock = Lock.new;

{
    my @r;
    my %h;
    race for ^4 -> $i {
        $lock.protect: {
            @r.push($i);
            %h{$i} = $i * 2;
        }
        own-r();
    }
    is @r.sort.join(","), "0,1,2,3", "race for: push to outer array";
    is %h.keys.sort.join(","), "0,1,2,3", "race for: element-assign outer hash";
    is %h.values.sort.join(","), "0,2,4,6", "race for: hash values";
}

{
    my @r = 0 xx 4;
    race for ^4 -> $i {
        @r[$i] = $i + 10;
    }
    is @r.join(","), "10,11,12,13", "race for: element-assign outer array";
}

{
    my @r;
    my %h;
    hyper for ^4 -> $i {
        $lock.protect: {
            @r.push($i);
            %h{$i} = $i + 1;
        }
        own-r();
    }
    is @r.sort.join(","), "0,1,2,3", "hyper for: push to outer array";
    is %h.values.sort.join(","), "1,2,3,4", "hyper for: element-assign outer hash";
}

{
    my @r = 1, 2;
    race for ^4 -> $i {
        $lock.protect: { @r = 5, 6, 7 };
    }
    is @r.join(","), "5,6,7", "race for: whole-assign outer array";
}

{
    my %h = a => 1;
    hyper for ^4 -> $i {
        $lock.protect: { %h = b => 2 };
    }
    is %h.keys.join(","), "b", "hyper for: whole-assign outer hash";
}

{
    my @r;
    race for ^4 -> $i {
        my @local;
        @local.push($i);
        $lock.protect: { @r.push(@local.elems) };
    }
    is @r.join(","), "1,1,1,1", "race for: body-local array stays per iteration";
}

is own-r(), 2, "routine with its own my \@r unaffected";
