use Test;

# #18575: a method `Nil` declares fails its signature when none of its
# candidates binds the arguments; only a name no candidate declares reaches
# `Nil`'s FALLBACK (which answers `Nil`).

plan 13;

throws-like { Nil.Str(1) }, X::Multi::NoMatch,
    message => /^ 'Cannot resolve caller Str(Nil:U: Int:D); none of these signatures matches:' /,
    'Str with a positional fails the multi dispatch';
ok (try { Nil.Str(1) } // $!.message).contains('(Nil $:: *%_ --> "")'),
    "... listing Nil's own candidate";
throws-like { Nil.gist(1) }, X::Multi::NoMatch,
    message => /^ 'Cannot resolve caller gist(Nil:U: Int:D)' /, 'gist with a positional';
throws-like { Nil.Numeric(1) }, X::AdHoc,
    message => 'Too many positionals passed; expected 1 argument but got 2',
    'Numeric is an only method';
throws-like { Nil.uc(1, 2, :x) }, X::AdHoc,
    message => 'Too many positionals passed; expected 1 argument but got 3',
    "uc's bounded proto refuses positionals";
throws-like { Nil.chars(1) }, X::AdHoc,
    message => 'Too many positionals passed; expected 1 argument but got 2', 'chars';
throws-like { Nil.chrs(1) }, X::AdHoc,
    message => 'Too many positionals passed; expected 1 argument but got 2', 'chrs';

# The candidates that do bind still answer.
my @warnings;
my @answers;
{
    @answers.push: Nil.chrs;
    @answers.push: Nil.Int(1);
    CONTROL { when CX::Warn { @warnings.push: .message; .resume } }
}
is-deeply @answers, ["\0", 0], 'chrs and Int(|) answer';
is-deeply @warnings, ['Use of Nil in numeric context' xx 2],
    'both warn about the numeric context';

# An undeclared name is absorbed.
is-deeply Nil.no-such-method(1, 2), Nil, 'FALLBACK answers Nil';

# `Nil.iterator` is the iterator of the one `Nil`.
my $iterator = Nil.iterator;
is-deeply $iterator.pull-one, Nil, 'iterator yields Nil';
ok $iterator.pull-one =:= IterationEnd, '... once';

# A hyper call reports the refusal too.
throws-like { (Nil,)».Str(1) }, X::Multi::NoMatch, 'a hyper call on a Nil element';
