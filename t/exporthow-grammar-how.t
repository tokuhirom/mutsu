use v6;
use Test;
use lib 't/lib';
use GrammarProfilerFixture;

# Pin for the Metamodel::GrammarHOW / EXPORTHOW custom-metaclass protocol
# (roast integration/advent2011-day07.t):
#  - `class X is Metamodel::GrammarHOW` (and ClassHOW) is inheritable
#  - `callsame` in a user find_method reaches the native default find_method
#  - EXPORTHOW.WHO.<grammar> makes later `grammar` declarations use the HOW
#  - subrule dispatch routes through find_method; the wrapper's $meth($cursor)
#    call runs the token and returns a Match

plan 8;

grammar ProfiledG {
    rule TOP { <num> +% <op> }
    token num { <[ 0..9 \. ]>+ }
    token op { < + - * / = > }
}

is ProfiledG.HOW.^name, 'ProfiledGrammarHOW', 'grammar declared under EXPORTHOW gets the custom HOW';

my $r = ProfiledG.parse("37 + 10 - 5 = 42");
ok ?$r, 'parse still succeeds under the profiling HOW';
is ~$r, "37 + 10 - 5 = 42", 'match covers the whole input';

my %t = get-timing();
ok %t<ProfiledG><num><calls>, 'num calls recorded';
ok %t<ProfiledG><op><calls>, 'op calls recorded';
ok %t<ProfiledG><num><time> ~~ Real:D, 'num time is a defined Real';

# find_method exclusions return the method unwrapped: parse itself not profiled
nok %t<ProfiledG><parse>:exists, 'excluded method names are not profiled';

# a token method value called with a cursor runs the token at that position
my class PlainHOW is Metamodel::GrammarHOW {
    method find_method($obj, $name) { callsame }
}
my $meth = PlainHOW.new.find_method(ProfiledG, "num");
my $m = $meth("42 x");
is ~$m, "42", 'token method value matches at the cursor position';

done-testing;
