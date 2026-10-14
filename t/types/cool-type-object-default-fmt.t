use Test;

plan 6;

for Str, Int, Num, Cool, List -> $t {
    my $r;
    {
        # the "uninitialized value" warning goes to $*ERR; only the value matters here
        $r = $t.fmt;
        CATCH { default { $r = 'died: ' ~ .^name } }
    }
    is $r, '', "{$t.^name}.fmt is the empty string";
}

is Int.fmt('[%5s]'), '[     ]', 'a type object with a format still formats the empty string';
