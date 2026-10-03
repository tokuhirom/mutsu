use Test;

# A `:D` / `:U` smiley and `.DEFINITE` test concreteness (`nqp::isconcrete`),
# not `.defined`: `Empty` and a `Failure` are concrete instances even though
# `.defined` is False for them. Found in highlighter, whose
# `multi sub matches(... --> Slip:D)` returns `Empty`.

plan 14;

sub empty-slip(--> Slip:D) { Empty }
is empty-slip().raku, 'Empty', 'Empty satisfies a Slip:D return type';
my Slip:D $s = Empty;
is $s.raku, 'Empty', 'Empty assigns to a Slip:D variable';
sub takes-slip(Slip:D $x) { 'bound' }
is takes-slip(Empty), 'bound', 'Empty binds to a Slip:D parameter';
ok Empty ~~ Slip:D, 'Empty ~~ Slip:D';
nok Empty ~~ Slip:U, 'Empty !~~ Slip:U';
ok Empty.DEFINITE, 'Empty.DEFINITE';
nok Empty.defined, 'Empty.defined is still False';
nok Slip.DEFINITE, 'the Slip type object is not DEFINITE';
ok Slip ~~ Slip:U, 'Slip ~~ Slip:U';

sub any-d(Any:D $x) { 'bound' }
my $f = Failure.new('x');
is any-d($f), 'bound', 'a Failure binds to an Any:D parameter';
ok $f ~~ Failure:D, 'Failure ~~ Failure:D';
nok $f.defined, 'Failure.defined is still False';
$f.handled = True;

multi m(Any:U) { 'U' }
multi m(Any:D) { 'D' }
is m(Empty), 'D', 'multi dispatch sends Empty to the :D candidate';
is m(Slip), 'U', 'and the Slip type object to the :U candidate';
