use v6;
use Test;

plan 6;

# A placeholder used ONLY as the target of an element assignment
# (`{ $^x<a> = v }`) must still make the block arity-1: the placeholder
# collectors walked `Expr::Index` (reads) but not `Expr::IndexAssign`, so the
# block compiled with arity 0, never bound its argument, and the assignment
# autovivified a private hash instead of dispatching ASSIGN-KEY on the
# argument (Text::CSV `on_in => { $^r<bar> = "" }`, 91_csv_cb.t tests 27-28).

class R {
    has %.h = (a => 1);
    method AT-KEY(Str $k) { %!h{$k} }
    method ASSIGN-KEY(Str $k, $v) { %!h{$k} = $v }
    method AT-POS(int $i) { $i }
    method ASSIGN-POS(int $i, $v) { %!h{"p$i"} = $v }
}

my $r = R.new;
my &cb = { $^x<a> = 3 };
is &cb.arity, 1, 'assign-target placeholder gives the block arity 1';
cb($r);
is $r.h<a>, 3, 'keyed element assignment reaches the argument (ASSIGN-KEY)';

my &cb2 = { $^x[1] = 9 };
cb2($r);
is $r.h<p1>, 9, 'positional element assignment reaches the argument (ASSIGN-POS)';

my &cb3 = { $^y<b> = $^x<a> };
is &cb3.arity, 2, 'placeholders in target and value both count';
cb3($r, $r);
is $r.h<b>, 3, 'both placeholders bound their arguments';

my %h = (k => 1);
my &cb4 = { $^m<k> = 42 };
cb4(%h);
is %h<k>, 42, 'plain hash argument is element-assigned too';
