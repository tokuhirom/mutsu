use Test;

plan 8;

# `is required` on a named parameter is exactly the `!` required marker: the
# call must die when the named argument is not passed, even when a slurpy `*%h`
# could otherwise absorb named arguments.

sub need-n(:$n is required) { $n }
lives-ok { need-n(n => 5) }, ':$n is required lives when n is passed';
is need-n(n => 5), 5, ':$n is required binds the value';
dies-ok { need-n() }, ':$n is required dies when n is missing';

# `is required` matches the `!` shorthand behaviour and message.
sub need-bang(:$n!) { $n }
{
    my $msg-trait = (try { need-n(); CATCH { default { .message } } }) // $!.message;
    my $msg-bang  = (try { need-bang(); CATCH { default { .message } } }) // $!.message;
    is $msg-trait, $msg-bang, 'is required and ! give the same error message';
}

# A slurpy `*%h` must not satisfy a required named parameter.
sub with-slurpy(:$n is required, *%h, *@a) { "$n {%h.elems} {@a.elems}" }
lives-ok { with-slurpy(1, n => 20, y => 300, 4000) },
    'required named lives when present alongside *%h/*@a';
is with-slurpy(1, n => 20, y => 300, 4000), '20 1 2',
    'required named binds; extras go to *%h and *@a';
dies-ok { with-slurpy(1, x => 20, y => 300, 4000) },
    'required named dies even when *%h could absorb the named args';

# Multiple required named params.
sub two(:$a! is required, :$b is required) { "$a$b" }
dies-ok { two(a => 1) }, 'all required named params are enforced';

# vim: expandtab shiftwidth=4
