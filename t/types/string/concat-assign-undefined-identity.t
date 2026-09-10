use v6;
use Test;

plan 8;

# `$x OP= $y` on an undefined `$x` seeds the container with `infix:<OP>`'s
# zero-arg identity, so `~=` starts from '' and emits no "uninitialized value in
# string context" warning. (`HTTP::Message.add-content` is `$.content ~= $c`.)

my @warnings;
my $collect = -> $w { @warnings.push: ~$w };

sub no-warnings(&code, $desc) {
    my @seen;
    {
        code();
        CONTROL { default { @seen.push: ~$_; $_.resume } }
    }
    is @seen.grep(*.contains('uninitialized')).elems, 0, $desc;
}

no-warnings { my $a; $a ~= "x"; }, 'plain scalar';
no-warnings { my Str $b; $b ~= "x"; }, 'typed scalar';
no-warnings { my @arr; @arr[0] ~= "x"; }, 'array element';
no-warnings { my %h; %h<z> ~= "x"; }, 'hash element';
no-warnings {
    my class K { has $.v is rw };
    my $k = K.new;
    $k.v ~= "x";
}, 'rw attribute through its accessor';

# The values are still right.
my $a; $a ~= "x"; $a ~= "y";
is $a, "xy", 'the concatenation result is correct';

my $n; $n += 3;
is $n, 3, 'numeric compound assignment keeps its identity too';

# A plain `~` on an undefined value DOES still warn.
my @seen;
{
    my $u;
    my $r = $u ~ "a";
    CONTROL { default { @seen.push: ~$_; $_.resume } }
}
ok @seen.grep(*.contains('uninitialized')).elems > 0,
    'plain infix ~ on an undefined value still warns';
