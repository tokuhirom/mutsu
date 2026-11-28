# A parse called on a grammar INSTANCE runs its start rule on `self.new`. With
# no user `new` in the MRO that is `Mu.new`: a fresh object of the instance's
# class with its attribute defaults, built by the same native constructor the
# type-object form uses (it used to go through the generic method-call path,
# ~54k instructions per parse). Values are rakudo's.
use Test;

plan 9;

grammar G {
    has $.x = 1;
    has @.log;
    token TOP { <word> { @*seen.push: self.x } }
    token word { \w+ }
}

{
    my @*seen;
    my $i = G.new(x => 5);
    my $m = $i.parse('abc');
    ok $m, 'an instance parse matches';
    is ~$m<word>, 'abc', 'its captures are there';
    is @*seen.join(','), '1', 'the start rule runs on a fresh object with the defaults';
    is $i.x, 5, 'the receiving instance is untouched';
    ok $m.WHAT === G, 'the Match is the grammar\'s cursor type';
}

class A { method word($/) { make ~$/ }; method TOP($/) { make $<word>.made } }
{
    my @*seen;
    is G.new.parse('xyz', :actions(A)).made, 'xyz', 'actions run for an instance parse';
    is G.new.subparse('xyz rest').to, 3, 'subparse from an instance';
}

grammar H is G { submethod TWEAK { @*seen.push: 'tweak' } }
{
    my @*seen;
    H.new.parse('q');
    is @*seen.join(','), 'tweak,tweak,1', 'a TWEAK runs for the receiver and for self.new';
}

grammar N { method new(|c) { @*seen.push: 'user-new'; callsame }; token TOP { a } }
{
    my @*seen;
    N.new.parse('a');
    is @*seen.join(','), 'user-new,user-new', 'a user new is still called on the instance';
}
