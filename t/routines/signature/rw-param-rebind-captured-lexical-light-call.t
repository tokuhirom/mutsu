use Test;

# A named sub that `:=`-binds a captured lexical to its `is rw` parameter
# shares the caller's container on every call, including the warm calls a
# light path serves: the parameter's slot already holds the argument's cell
# (ADR-0109), and the bind joins that cell instead of minting a copy.

plan 8;

{
    my $outer;
    sub rebind($arg is rw) { $outer := $arg }
    for ^3 -> $round {
        my $v = 10 * ($round + 1);
        rebind($v);
        $v++;
        if $round == 2 {
            is $outer, 31, 'a warm call: the rebound lexical sees the caller variable';
            $outer = 99;
            is $v, 99, 'a warm call: a write through the rebound lexical reaches the caller';
        }
    }
}

my $top;
sub bind-top($x is rw) { $top := $x }
my @seen;
for 1..3 -> $i {
    my $n = $i;
    bind-top($n);
    $n *= 100;
    @seen.push: $top;
}
is @seen.join(','), '100,200,300', 'a mainline lexical follows each call\'s argument';

sub twice($x is rw) { $top := $x; $top++ }
my $w = 5;
twice($w) for ^3;
is $w, 8, 'increments through the rebound lexical reach the argument';
is $top, 8, 'and the lexical reads the argument';
$w = 1;
is $top, 1, 'a later write to the argument shows through the lexical';

# A readonly parameter still binds a snapshot.
sub bind-ro($x) { $top := $x }
my $r = 7;
bind-ro($r) for ^3;
$r = 8;
is $top, 7, 'a readonly parameter binds its value, not the caller container';
dies-ok { $top = 1 }, 'and the bound value is readonly';
