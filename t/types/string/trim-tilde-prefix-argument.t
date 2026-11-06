use Test;

plan 15;

# A named unary builtin takes a glued `~`/`-`/`+` prefix term as its argument:
# `trim ~$x` is `trim(~$x)`, not the bareword `trim` concatenated with `~$x`
# (found via Jupyter::Kernel's `Magic::Run.new(file => trim ~$<rest>)`).
my $r = " abc ";
my $n = 5;
my %h = rest => " xyz ";

is (trim ~$r), "abc", 'trim ~$var';
is (trim ~%h<rest>), "xyz", 'trim ~%h<key>';
is (trim-leading ~$r), "abc ", 'trim-leading ~$var';
is (trim-trailing ~$r), " abc", 'trim-trailing ~$var';
is (uc ~$r), " ABC ", 'uc ~$var';
is (lc ~$n), "5", 'lc ~$var';
is (flip ~$n), "5", 'flip ~$var';
is (chop ~$r), " abc", 'chop ~$var';
is (chomp ~$r), " abc ", 'chomp ~$var';
is (tclc ~$r), " abc ", 'tclc ~$var';
is-deeply (ords ~$r).List, (32, 97, 98, 99, 32), 'ords ~$var';
is (chrs ~$n), "\x[5]", 'chrs ~$var';

class A { has $.file }
is A.new(file => trim ~$r).file, "abc", 'as a named argument value';

# A user routine and a spaced infix are unaffected.
sub twice($x) { $x ~ $x }
is (twice ~$r), " abc  abc ", 'a user sub with a glued ~ argument';
my $uc = "U";
is ($uc ~ $r), "U abc ", 'a variable named like the builtin stays a term';
