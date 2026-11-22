use v6;
use Test;
use lib 't/lib';

plan 6;

# An import tag the module does not declare is X::Import::NoSuchTag, at compile
# time, whatever tags come with it: `:ALL` beside it does not excuse it
# (roast S11-modules/re-export.t's `use InnerModule :ALL, :EXPORT`).
sub out-of(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', 't/lib', '-e', $code, :out, :err;
    ($proc.out.slurp(:close), $proc.err.slurp(:close))
}

for 'use UnknownTagM :ALL, :NOPE;', 'use UnknownTagM :NOPE;', 'use UnknownTagM :DEFAULT, :NOPE;' -> $use {
    my ($out, $err) = out-of("say 'ran'; $use");
    ok $out eq '' && $err.contains("Error while importing from 'UnknownTagM': no such tag 'NOPE'"),
        "$use is a compile-time no-such-tag error";
}

my ($out) = out-of('use UnknownTagM :ALL; say ut-foo, ut-bar;');
is $out, "foobar\n", ':ALL alone imports every tag';

throws-like 'use UnknownTagM :ALL, :NoSucTag;', X::Import::NoSuchTag,
    :source-package<UnknownTagM>, :tag<NoSucTag>,
    'the exception names the module and the tag';
throws-like 'use UnknownTagM :NoSucTag;', X::Import::NoSuchTag,
    message => "Error while importing from 'UnknownTagM': no such tag 'NoSucTag'",
    'the message is rakudo\'s';
