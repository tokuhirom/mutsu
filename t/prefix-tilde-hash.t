use Test;

plan 3;

my %h = a => 1, b => 2;
ok (~%h).chars > 0, 'prefix ~ on hash produces string';

my %h2 = x => 10;
is ~%h2, "x\t10", 'prefix ~ on single-pair hash';

# prefix ~ on hash method call
my %h3 = a => 1, b => 2;
ok (~%h3.sort).chars > 0, 'prefix ~ on hash method call';
