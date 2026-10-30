use Test;

# From Archive::Libarchive (Archive::Libarchive::Entry.filetype): a `where`
# clause that is a bare colonpair (`where :!so`, `where :so`) smartmatches the
# argument against the Pair, i.e. calls the method the key names and compares
# its truthiness with the pair's value. The binder's smartmatch lacked that arm
# (only the VM's had it), so such a parameter rejected every argument.
plan 10;

sub no(:$d where :!so) { "no" }
sub yes($d where :so) { "yes" }
sub no-pos($d where :!so) { "no-pos" }

is no(), 'no', 'an omitted optional named parameter satisfies where :!so';
is no(:d(0)), 'no', 'a false value satisfies where :!so';
dies-ok { no(:d(1)) }, 'a true value does not';
is yes(1), 'yes', 'a true value satisfies where :so';
dies-ok { yes(0) }, 'a false value does not';
is no-pos(Any), 'no-pos', 'a type object satisfies where :!so';

class E {
    multi method ft(Int $t) { "set $t" }
    multi method ft(:$decode where :!so --> Int) { 42 }
    multi method ft(:$decode where :so --> List) { (1, 2) }
}
my $e = E.new;
is $e.ft(3), 'set 3', 'multi method: the positional candidate';
is $e.ft, 42, 'multi method: no :decode picks where :!so';
is-deeply $e.ft(:decode), (1, 2), 'multi method: :decode picks where :so';
is $e.ft(:!decode), 42, 'multi method: :!decode picks where :!so';
