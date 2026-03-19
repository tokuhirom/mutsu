use Test;

plan 3;

my %h = a => { b => 42 };
is %h.{"a"}{"b"}, 42, '.{...} indexes hashes';

my @a = [10, 20, 30];
dies-ok { EVAL '@a.{1}' }, '.{...} on arrays throws (associative indexing not supported)';

my $hash = { a => { b => { c => 7 } } };
sub walk (Hash $h, Str $k) { $h.{$k} }
is (produce(&walk, flat $hash, <a b c>)).gist,
   '({a => {b => {c => 7}}} {b => {c => 7}} {c => 7} 7)',
   '.{...} works in produce callback chain';
