use Test;

plan 6;

{
    sub f(@arr is copy) {
        @arr.push(99);
        @arr.join(',');
    }

    my @arr = 1, 2;
    is f(@arr), '1,2,99', 'array is copy uses a writable local copy';
    is @arr.join(','), '1,2', 'array is copy leaves caller array unchanged';
}

{
    sub g(%h is copy) {
        %h<new> = 1;
        %h<a> = 20;
        %h.elems ~ '|' ~ %h<a> ~ '|' ~ %h<new>;
    }

    my %h = :a(10);
    is g(%h), '2|20|1', 'hash is copy uses a writable local copy';
    is %h.elems, 1, 'hash is copy does not add keys to caller hash';
    is %h<a>, 10, 'hash is copy does not overwrite caller values';
    ok !(%h<new>:exists), 'hash is copy does not leak new entries to caller';
}
