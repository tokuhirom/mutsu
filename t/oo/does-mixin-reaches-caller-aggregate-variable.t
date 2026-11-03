use Test;

# `h does R` inside a routine reblesses the caller's Hash/Array variable
# (#16231, from Hash::Util's lock_hash(%hash)).

plan 14;

role L { method who { "L" } }

sub a(\h)            { h does L; h }
sub b($h is raw)     { $h does L; $h }
sub c(%h)            { %h does L; %h }
sub e(\v)            { v does L; v }
sub ca(@h)           { @h does L; @h }
sub nop(\g)          { 1 }
sub pass-first(\h)   { nop(h); h does L; 1 }
sub typed(Associative:D \h) { (h does L).who }

my %x = a => 1; a(%x);
ok %x ~~ L, 'sigilless param: Hash variable does the role';
my %y = a => 1; b(%y);
ok %y ~~ L, 'is raw scalar param: Hash variable does the role';
my %z = a => 1; c(%z);
ok %z ~~ L, 'plain % param: Hash variable does the role';
my @arr = 1, 2; e(@arr);
ok @arr ~~ L, 'sigilless param: Array variable does the role';
my @arr2 = 1, 2; ca(@arr2);
ok @arr2 ~~ L, 'plain @ param: Array variable does the role';
my $s = "str"; e($s);
ok $s ~~ L, 'scalar variable still does the role';
my $o = Any.new; e($o);
ok $o ~~ L, 'object variable still does the role';

my %p = a => 1; pass-first(%p);
ok %p ~~ L, 'the parameter was passed on to another call before the does';
my %t = a => 42; is typed(%t), "L", 'typed sigilless param, parenthesised does';
ok %t ~~ L, 'typed sigilless param: caller variable does the role';

is %x<a>, 1, 'the Hash keeps its contents';
is %x.elems, 1, 'and its size';
is @arr.elems, 2, 'the Array keeps its size';
is %x.who, "L", 'the role method is callable on the caller variable';

done-testing;
