use Test;

plan 6;

sub check-depth($open, $close, $depth) {
    my $source = ($open x $depth) ~ '1' ~ ($close x $depth);
    try EVAL($source);
    ok !$!.defined || $!.message.contains('Too deep recursion'),
        "deeply nested $open input succeeds or raises a catchable depth error";
    pass 'execution continues after deeply nested input';
}

check-depth('(', ')', 2000);
check-depth('{', '}', 1000);
check-depth('[', ']', 1000);
