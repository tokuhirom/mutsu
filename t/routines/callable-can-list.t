use Test;

plan 4;

sub plain($value) { $value }

is &plain.can('IS_PURE').elems, 0,
        'a callable reports an absent method as an empty list';
ok !&plain.can('IS_PURE'),
        'an absent callable method remains false in boolean context';
is &plain.can('name').elems, 1,
        'a callable reports a supported method as one list element';
ok &plain.can('name'),
        'a supported callable method remains true in boolean context';

done-testing;
