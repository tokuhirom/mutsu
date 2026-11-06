use Test;

# The routine form of a Cool routine is the method form: handing it an
# unhandled Failure throws the Failure's exception (`chars $f` is `$f.chars`),
# it does not stringify the Failure's message.
my @names = <chars chr sqrt abs floor ceiling round sign exp log sin cos tan
             lc uc fc tc flip trim chomp chop ord lines words>;
plan @names.elems + 2;

for @names -> $n {
    my $code = 'sub mk { "ab" + 1 }; my $r = try { ' ~ $n ~ '(mk()) }; $!.^name';
    is (EVAL $code), 'X::Str::Numeric', "{$n}(Failure) throws the Failure's exception";
}

sub mk { "ab" + 1 }
my $f = mk();
dies-ok { chars $f }, 'chars $variable holding a Failure dies';
is (try { uc 'abc' }), 'ABC', 'an ordinary argument is unaffected';
