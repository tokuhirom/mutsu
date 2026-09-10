use Test;

plan 14;

# A compile-time error caught from EVAL must present its real exception class,
# not a generic X::AdHoc. `throws-like` and a `CATCH`/`when` both dispatch on
# the type, so an error that only spells its class inside the message text is
# indistinguishable from `die "..."`.

throws-like { EVAL q{"a" . "b"} }, X::Obsolete, 'the . concatenation braino';
throws-like { EVAL q{<>} }, X::Obsolete, 'the degenerate diamond';
throws-like { EVAL q{$& = 1} }, X::Syntax::Perl5Var, 'a Perl 5 special variable';
throws-like { EVAL q{my $x is readonly = 1} }, X::Comp::Trait::Unknown,
    'an unknown variable trait';

# The message keeps only the text: the class name is not repeated in it.
{
    my $e = (try { EVAL q{"a" . "b"} }, $!).tail;
    isa-ok $e, X::Obsolete, 'caught in $! with its class';
    nok $e.message.starts-with('X::'), 'the class name is not part of the message';
    ok $e.message.contains('concatenate'), 'and the text survives';
}

# A CATCH block dispatches on the type.
{
    my $seen;
    {
        EVAL q{$& = 1};
        CATCH {
            when X::Syntax::Perl5Var { $seen = 'perl5var' }
            default { $seen = 'default' }
        }
    }
    is $seen, 'perl5var', 'a typed when matches a compile error';
}

# An untyped error is still X::AdHoc, which IS-A Exception.
{
    my $e = (try { die 'plain failure' }, $!).tail;
    isa-ok $e, X::AdHoc, 'an untyped die is X::AdHoc';
    is $e.message, 'plain failure', 'with its message intact';
    isa-ok $e, Exception, 'and X::AdHoc IS-A Exception';
}

# A sentence that merely opens with `X::` is not a class name.
{
    my $e = (try { die 'X:: is the exception namespace' }, $!).tail;
    isa-ok $e, X::AdHoc, 'a message that only mentions X:: stays X::AdHoc';
    is $e.message, 'X:: is the exception namespace', 'and is not truncated';
}

# The same rule reaches a Failure produced by `fail`.
{
    sub obsolete-ish { fail X::Obsolete.new(old => '.', replacement => '~') }
    my $f = obsolete-ish;
    isa-ok $f.exception, X::Obsolete, 'a failed Failure keeps its class';
}
