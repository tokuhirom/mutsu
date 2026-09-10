use Test;

plan 1;

eval-lives-ok q:to/END/, 'pointy hash sub-signature parses under hyper chain with unspace';
    sub test() {
        my regex suffix { <[dhms]> };
        my %unit-multipliers = 's' => 1;
        <s>»\
            .match(/<suffix>/)».hash\
            .map(-> % ( Str(Any) :$suffix ) { %unit-multipliers{$suffix} });
    }
    test();
    END
