use Test;

# A `.=` declaration under a statement modifier declares the variable
# whatever the condition; only the call is gated, as for `=`
# (CSS::Specification::Compiler: `my RakuAST::ArgList $args .= new(|@args)
# if @args`).

plan 5;

my Int $a .= new(1) if False;
ok $a === Int, 'a false if leaves the type object';
my Int $b .= new(3) if True;
is $b, 3, 'a true if runs the call';
my Str $c .= new unless True;
ok $c === Str, 'unless';
my $d .= new if False;
ok $d === Any, 'the untyped form';
my RakuAST::ArgList $args .= new(1) if False;
ok $args === RakuAST::ArgList, 'a RakuAST type';
