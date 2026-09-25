use Test;

# `temp` / `let` of a brace-subscripted hash element (`temp %h{$k} = v`) parse
# and restore like the angle and bracket forms (Pod::To::PDF::Lite writes
# `temp %!replacing{$place-holder} = True`).

plan 12;

my %h = a => 0;
my $k = 'a';
{
    temp %h{$k} = 1;
    is %h<a>, 1, 'temp %h{$k} = v assigns';
}
is %h<a>, 0, 'temp %h{$k} restores at scope exit';

{
    temp %h{'a'};
    %h<a> = 7;
    is %h<a>, 7, 'bare temp %h{...} then assignment';
}
is %h<a>, 0, 'bare temp %h{...} restores';

class Rep {
    has %!replacing;
    method run($key) {
        {
            temp %!replacing{$key} = True;
            return %!replacing{$key} ?? 'set' !! 'unset';
        }
    }
    method after($key) { %!replacing{$key} // 'gone' }
}
my $r = Rep.new;
is $r.run('x'), 'set', 'temp on an attribute hash element';
is $r.after('x'), 'gone', 'attribute hash element restored';

my @a = 1, 2;
{ temp @a[0] = 9; is @a[0], 9, 'temp @a[i] still works' }
{ temp %h<a> = 5; is %h<a>, 5, 'temp %h<k> still works' }

# A compound assignment to the temporized element (`temp %header<k> //= v`,
# Net::HTTP) saves the element and then runs the assignment.
my %hdr = a => 1;
{
    temp %hdr<a> += 5;
    is %hdr<a>, 6, 'temp %h<k> += v';
}
is %hdr<a>, 1, 'temp %h<k> += v restores';
{
    temp %hdr<b> //= 'keep-alive';
    is %hdr<b>, 'keep-alive', 'temp %h<k> //= v on a missing key';
}
my @arr = 1, 2;
{ temp @arr[0] ~= 'x'; is @arr[0], '1x', 'temp @a[i] ~= v' }
