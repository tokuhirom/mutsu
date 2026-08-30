use Test;

# `do whenever` is the legal expression form of the statement control word.
# It must yield the subscription Tap on the ordinary value stack, irrespective
# of whether the source is a lexical Supply or a computed `.Supply` call.

plan 8;

{
    my $supplier = Supplier.new;
    my $s = $supplier.Supply;
    react {
        my $tap = do whenever $s -> $value { }
        isa-ok $tap, Tap, 'react do-whenever over a lexical Supply yields Tap';
        isa-ok $s, Supply, 'react do-whenever does not clobber its source';
        done;
    }
}

{
    my $supplier = Supplier.new;
    react {
        my $tap = do whenever $supplier.Supply -> $value { }
        isa-ok $tap, Tap, 'react do-whenever over a method call yields Tap';
        done;
    }
}

{
    my $supplier = Supplier.new;
    my $s = $supplier.Supply;
    react {
        my $tap = do { whenever $s -> $value { } }
        isa-ok $tap, Tap, 'braced do-whenever yields Tap';
        isa-ok $s, Supply, 'braced do-whenever does not clobber its source';
        done;
    }
}

{
    my $supplier = Supplier.new;
    my $s = $supplier.Supply;
    my $outer = supply {
        my $tap = do whenever $s -> $value { }
        isa-ok $tap, Tap, 'supply do-whenever over a lexical Supply yields Tap';
        isa-ok $s, Supply, 'supply do-whenever does not clobber its source';
        my $method-tap = do whenever $supplier.Supply -> $value { }
        isa-ok $method-tap, Tap, 'supply do-whenever over a method call yields Tap';
    };
    react {
        whenever $outer { }
        whenever Promise.in(0.01) { done }
    }
}
