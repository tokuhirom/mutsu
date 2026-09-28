use Test;

plan 10;

# https://github.com/tokuhirom/mutsu/issues/9827 -- a Bool matcher used to
# throw a bare X::Match::Bool with no `.type` attribute and the wrong
# message text ("with '.match'", missing the routine name).

sub type-and-message(&code) {
    my ($type, $message);
    try {
        code();
        CATCH {
            default {
                $type = .type;
                $message = .Str;
            }
        }
    }
    ($type, $message);
}

my ($type, $message) = type-and-message({ (1,2).grep(True) });
is $type, '.grep', '.grep method form: X::Match::Bool.type is ".grep"';
is $message,
    "Cannot use Bool as Matcher with '.grep'.  Did you mean to use \$_ inside a block?",
    '.grep method form: X::Match::Bool message names the routine';

($type, $message) = type-and-message({ grep True, 1, 2 });
is $type, '.grep', 'grep sub form: X::Match::Bool.type is ".grep"';
is $message,
    "Cannot use Bool as Matcher with '.grep'.  Did you mean to use \$_ inside a block?",
    'grep sub form: X::Match::Bool message names the routine';

($type, $message) = type-and-message({ (1,2).first(True) });
is $type, '.first', '.first method form: X::Match::Bool.type is ".first"';
is $message,
    "Cannot use Bool as Matcher with '.first'.  Did you mean to use \$_ inside a block?",
    '.first method form: X::Match::Bool message names the routine';

($type, $message) = type-and-message({ first True, 1, 2 });
is $type, '.first', 'first sub form: X::Match::Bool.type is ".first"';
is $message,
    "Cannot use Bool as Matcher with '.first'.  Did you mean to use \$_ inside a block?",
    'first sub form: X::Match::Bool message names the routine';

# The rw grep path (an `is rw` loop var forces element-by-element mutation)
# goes through a different throw site than the plain read-only grep above.
($type, $message) = type-and-message({
    my @a = 1, 2, 3;
    for @a.grep(True) -> $x is rw { $x++ }
});
is $type, '.grep', 'rw grep path: X::Match::Bool.type is ".grep"';

is (X::Match::Bool.new(type => '.grep').message),
    "Cannot use Bool as Matcher with '.grep'.  Did you mean to use \$_ inside a block?",
    'a hand-built X::Match::Bool.new(:type<.grep>) renders the same message';
