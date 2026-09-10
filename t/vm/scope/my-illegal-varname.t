use Test;

plan 10;

# Numeric variables (match-capture names) cannot be declared.
throws-like 'my $0', X::Syntax::Variable::Numeric,
    'my $0 cannot declare a numeric variable';
throws-like 'state $3', X::Syntax::Variable::Numeric,
    'state $3 cannot declare a numeric variable';

# Match variables cannot be declared.
throws-like 'my $<a>', X::Syntax::Variable::Match,
    'my $<a> cannot declare a match variable';

# `?` and `!` twigils cannot be used on a my/our/state variable.
throws-like 'my $?FILE', X::Syntax::Variable::Twigil, twigil => '?', scope => 'my',
    'my $?FILE rejects the ? twigil';
throws-like 'my $?LINE', X::Syntax::Variable::Twigil, twigil => '?', scope => 'my',
    'my $?LINE rejects the ? twigil';
throws-like 'my class A { my $!foo }', X::Syntax::Variable::Twigil,
    twigil => '!', scope => 'my',
    'my $!foo rejects the ! twigil';

# Reading these special forms is still fine, and ordinary declarations work.
lives-ok { EVAL 'my $x = 5; $x' }, 'an ordinary scalar declaration still works';
lives-ok { EVAL 'my $*dyn = 5; $*dyn' }, 'a dynamic variable declaration still works';
lives-ok { EVAL 'my $a::b' }, 'a package-qualified declaration still works';
lives-ok { EVAL 'say $?FILE.defined' }, 'reading $?FILE is still allowed';
