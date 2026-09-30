use Test;

# Distilled from CSS::TagSet (CSS::Module's `has Hash %.property-metadata`,
# constructed from an itemized `${...}` hash).
plan 5;

class M {
    has %.pm;
    method reg($k, $v) { %!pm{$k} = $v }
}

my $h = ${ a => 1, b => 2 };
my $m = M.new(:pm($h));
is $m.pm.raku, '{:a(1), :b(2)}', 'a %-attribute built from an itemized hash is the hash itself';
$m.reg('a', 5);
is $m.pm.elems, 2, 'element assignment through %!attr keeps the other keys';
is $m.pm<a>, 5, 'the assigned element is stored';
is $m.pm<b>, 2, 'the untouched element survives';
is M.new(pm => ${ z => 1 }).pm.raku, '{:z(1)}', 'a literal ${...} argument is de-itemized too';
