use Test;

# Distilled from the TAP distribution (parse-stream): a `my Str @buffer`
# captured by a named sub inside a `supply` block is reassigned from a
# `whenever`; the element constraint must not be applied to the whole array.
plan 4;

my sub stream(Supply $input --> Supply) {
    supply {
        my Str @buffer;
        sub reset($line) {
            emit $line;
            @buffer = ();
        }
        whenever $input -> $line {
            reset $line;
        }
    }
}

my @got;
stream(supply { emit "x"; emit "y" }).tap: { @got.push: $_ };
is-deeply @got, ["x", "y"], 'repeated whole-array reset inside whenever';

sub typed {
    my Int @a;
    my &f = sub { @a = 1, 2 };
    f();
    my $r = try { @a = "x", "y"; 1 };
    ($r, @a.elems);
}
my ($r, $n) = typed();
ok !$r, 'element type still enforced on whole assignment';
is $n, 2, 'array unchanged after failed assignment';

my Str %h;
sub clear { %h = () }
%h<a> = "b";
clear();
is %h.elems, 0, 'typed hash reset from a named sub';
