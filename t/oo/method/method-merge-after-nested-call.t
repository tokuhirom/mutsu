use Test;

plan 6;

# A method that calls other methods merges its frame's writes back to the
# caller env: caller lexicals the method writes stay visible, its own locals
# and parameters do not leak (#16098).
my $counter = 0;
my @log;
class Acc {
    has $.n = 0;
    method bump { $counter++; $!n++; self }
    method twice { self.bump; self.bump; $!n }
    method local-only($x) { my $tmp = $x * 2; self.bump; $tmp }
    method push-log($m) { @log.push($m); self.bump; @log.elems }
}

my $a = Acc.new;
is $a.twice, 2, 'a method calling a method returns the attribute';
is $counter, 2, 'a caller lexical written through nested method calls is merged back';
my $tmp = 'outer';
is $a.local-only(21), 42, 'a method local is computed';
is $tmp, 'outer', 'a method-local name does not clobber a caller lexical of the same name';
is $a.push-log('x'), 1, 'a caller array written by a method is visible';
is-deeply @log, ['x'], 'the push is merged back';
