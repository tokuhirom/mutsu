use Test;

# A bare block's `}` at the end of its line ends the statement, so a
# `.method` on the next line is a new statement calling the method on `$_`,
# not a method call on the block (#18385: Red's `^create` ended a nested
# block with `{ ... }\n.return with $no` and returned a Block).

plan 6;

sub ret-with() {
    my $no = 42;
    { my $x = 2; }
    .return with $no;
    5
}
is ret-with(), 42, '.return with $x after a bare block returns $x';

sub nested() {
    {
        my $no = 'row';
        {
            my $*X = 2;
        }
        .return with $no;
        'proxy'
    }
}
is nested(), 'row', 'same inside a nested bare block';

class M {
    multi method create(\model, *%orig) is rw {
        KEEP { my $k = 1 }
        {
            my $no;
            $no = 7 if %orig;
            {
                my $g = 3;
            }
            .return with $no;
            return-rw Proxy.new:
                STORE => -> | { die 'ro' },
                FETCH => { 9 }
        }
    }
}
is M.create(M, :a).^name, 'Int', 'is rw multi method returns the value, not a Block';
is M.create(M, :a), 7, '... and the right value';

my @seen;
$_ = 'topic';
{ my $u = 1 }
.&{ @seen.push($_) };
is-deeply @seen, ['topic'], 'next-line .&{} calls on the topic';

my @calls;
{ @calls.push('a') }.();
{ @calls.push('b') }();
is-deeply @calls, ['a', 'b'], 'same-line .() and () still call the block';
