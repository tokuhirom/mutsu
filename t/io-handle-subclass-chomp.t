use v6;
use Test;

# IO::Handle has `$.chomp is rw = True`; a pure-Raku subclass created via
# bless carries no native handle state, but the inherited accessor must still
# read (default True) and write. (Text::CSV's 85_util.t: Text::IO::String
# `is IO::Handle`, and getline does `my Bool $chomped = $io.chomp`.)

plan 5;

class MyIO is IO::Handle { }

my $io = MyIO.new;
is $io.chomp, True, 'inherited chomp reads the IO::Handle default (True)';
ok $io.chomp ~~ Bool, 'default is a Bool, not a Str coercion of the invocant';

$io.chomp = False;
is $io.chomp, False, 'chomp is writable through the inherited rw accessor';
$io.chomp = True;
is $io.chomp, True, 'and writable back';

# A user-declared chomp method outranks the inherited accessor.
class Custom is IO::Handle {
    method chomp { "custom" }
}
is Custom.new.chomp, "custom", 'user-declared chomp method wins';

done-testing;
