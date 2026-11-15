use Test;

plan 4;

my $source = 1;
class Holder {
    has Mu $!value;
    method bind() { $!value := $source }
    method raw-value() is raw { $!value }
}

my $holder = Holder.new;
$holder.bind;
$holder.raw-value = 5;
is $source, 5, 'writing through a raw attribute reaches its bound outer scalar';

$source = 8;
is $holder.raw-value, 8, 'the attribute sees later writes to the source scalar';

my $other = 10;
class OtherHolder {
    has Mu $!value;
    method bind() { ($!value := $other) }
    method raw-value() is raw { $!value }
}

my $other-holder = OtherHolder.new;
$other-holder.bind;
$other-holder.raw-value = 12;
is $other, 12, 'expression-form bind also captures the outer scalar cell';

$other = 14;
is $other-holder.raw-value, 14, 'expression-form bind keeps the source live';
