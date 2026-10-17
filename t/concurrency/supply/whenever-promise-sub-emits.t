use Test;

plan 1;

my $s = supply {
    sub flush() { emit 42 }
    whenever Promise.in(0.01) {
        flush();
        done;
    }
};
my @seen;
react {
    whenever $s -> $value { @seen.push($value) }
}

is-deeply @seen, [42], 'bare emit in a sub called from a Promise whenever reaches its supply';
