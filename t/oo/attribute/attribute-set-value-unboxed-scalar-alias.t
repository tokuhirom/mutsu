use Test;

plan 12;

class Holder {
    has Mu $!value;
    method raw-value() is raw { $!value }
}

my $direct = 1;
my $holder = Holder.new;
Holder.^attributes[0].set_value($holder, $direct);
$holder.raw-value = 5;
is $direct, 5, 'a direct call binds an unboxed caller scalar';
$direct = 8;
is $holder.raw-value, 8, 'writes through the caller update the attribute';

my $meta = Holder.^attributes[0];
my $variable = 2;
$meta.set_value($holder, $variable);
$holder.raw-value = 6;
is $variable, 6, 'a variable metaobject receiver binds the caller scalar';
$variable = 9;
is $holder.raw-value, 9, 'a variable receiver shares the cell both ways';

my $outer = 3;
my $bind-outer = { $meta.set_value($holder, $outer) };
$bind-outer();
$holder.raw-value = 7;
is $outer, 7, 'a captured outer scalar is promoted in its owner';
$outer = 10;
is $holder.raw-value, 10, 'the captured outer cell stays shared';

my $shadow = 1;
{
    my $shadow = 2;
    $meta.set_value($holder, $shadow);
    $holder.raw-value = 4;
    is $shadow, 4, 'a shadowed local receives the attribute write';
}
is $shadow, 1, 'the outer homonym remains separate';

$meta.set_value($holder, 11);
is $holder.raw-value, 11, 'a literal value is still bound by value';

my @array = 1, 2;
$meta.set_value($holder, @array);
is $holder.raw-value.VAR.^name, 'Array', 'an Array argument remains its own container';

my $bound := 12;
$meta.set_value($holder, $bound);
is $holder.raw-value.VAR.^name, 'Int', 'a value bound with := has no Scalar';

class Other {
    method set_value($object, $value) { $value.^name }
}
is Other.new.set_value($holder, $direct), 'Int',
    'a user method with the same name receives an ordinary argument';
