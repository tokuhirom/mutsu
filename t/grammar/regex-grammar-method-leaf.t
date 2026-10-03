use Test;

# A `<name>` call that resolves to a plain grammar METHOD (no rule of that
# name) runs on the compiled regex engine as a leaf: the method is called once
# on the calling rule's cursor and answers at most one end (ADR-0135 §8,
# Slice E, eighteenth part). Expected values are rakudo's.

plan 12;

grammar ZeroWidth {
    token TOP { 'a' <.mark> 'b' }
    method mark { self }
}
is ZeroWidth.parse('ab').Str, 'ab', 'a method returning self is a zero-width success';
nok ZeroWidth.parse('ac'), 'the rest of the rule still has to match after it';

grammar Args {
    token TOP { <.expect('x')> 'y' }
    token alt { <.expect: 'p'> 'q' }
    method expect($what) {
        self.orig.substr(self.pos, $what.chars) eq $what
            ?? self.new(:orig(self.orig), :pos(self.pos + $what.chars))
            !! self.new(:pos(-1))
    }
}
ok Args.parse('xy'), 'a method called with parenthesized arguments';
ok Args.parse('pq', :rule<alt>), 'a method called with colon arguments';
nok Args.parse('zy'), 'a cursor with a negative pos is a failed call';

grammar Extent {
    token TOP { <.digits> '!' }
    method digits { self.orig.substr(self.pos) ~~ /^ \d+ / }
}
is Extent.parse('123!').Str, '123!', 'a returned Match advances the parse by its extent';

grammar Dies {
    token TOP { 'a' <.panic> }
    method panic { die "stopped here" }
}
throws-like { Dies.parse('ab') }, Exception, message => 'stopped here',
    'the exception of the method ends the parse';

my $calls = 0;
grammar Backtrack {
    regex TOP { 'a'* <.count> 'ab' }
    method count { $calls++; self }
}
ok Backtrack.parse('aaab'), 'a regex backtracks across a method call';
is $calls, 2, 'the method runs once per position the call is reached at';

grammar Noted {
    has $.seen;
    token TOP { <t> }
    token t { 'a' <.note> }
    method note { $!seen = 'yes'; self }
}
my $m = Noted.parse('a');
is $m<t>.seen, 'yes', 'the method runs on the calling rule invocation\'s cursor';
nok $m.seen.defined, 'the caller\'s own cursor was not written';

grammar Capturing {
    token TOP { <word> }
    method word { self.orig.substr(self.pos) ~~ /^ \w+ / }
}
is Capturing.parse('hello').Str, 'hello', 'a capturing call of a method matches';
