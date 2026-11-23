# Topic (`$_`) reads in routines of a package: blocks, for, given, with,
# a routine's own topic and a nested sub's, served by GetGlobal's env
# shortcut outside GLOBAL too (#18274).
use Test;
module M {
    our sub doubled(@xs) { @xs.map({ $_ * 2 }).List }
    our sub summed(@xs) { my $s = 0; for @xs { $s += $_ }; $s }
    our sub given-it($x) {
        given $x {
            when Int { 'int ' ~ $_ }
            default { 'other ' ~ $_ }
        }
    }
    our sub with-it($x) { with $x { "got $_" } else { 'none' } }
    our sub own-topic() { $_ = 'mine'; $_ }
    our sub nested() {
        my sub inner($y) { $_ = $y; $_ ~ '!' }
        for <a b> { inner($_ ~ $_) }
        $_ // 'unset'
    }
    our sub topic-of-caller() { $_ }
}
is-deeply M::doubled([1, 2, 3]), (2, 4, 6), 'a block topic in a package routine';
is M::summed([1, 2, 3]), 6, 'a for topic in a package routine';
is M::given-it(5), 'int 5', 'given topicalizes';
is M::given-it('x'), 'other x', 'and its default sees it';
is M::with-it(7), 'got 7', 'with topicalizes';
is M::with-it(Nil), 'none', 'without';
is M::own-topic(), 'mine', "a routine's own topic";
is M::nested(), 'unset', "a nested sub's topic does not leak into its caller's";
$_ = 'outer';
nok M::topic-of-caller().defined, "a routine does not see its caller's topic";
is $_, 'outer', 'the mainline topic is untouched';
done-testing;
