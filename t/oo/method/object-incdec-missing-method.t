use Test;

class NoStep { }
my $object = NoStep.new;
my $original = $object.WHICH;
throws-like { $object++ }, X::Method::NotFound, method => 'succ', 'postfix increment requires succ';
is $object.WHICH, $original, 'failed postfix increment leaves the variable unchanged';
throws-like { ++$object }, X::Method::NotFound, method => 'succ', 'prefix increment requires succ';
is $object.WHICH, $original, 'failed prefix increment leaves the variable unchanged';
throws-like { $object-- }, X::Method::NotFound, method => 'pred', 'postfix decrement requires pred';
is $object.WHICH, $original, 'failed postfix decrement leaves the variable unchanged';
throws-like { --$object }, X::Method::NotFound, method => 'pred', 'prefix decrement requires pred';
is $object.WHICH, $original, 'failed prefix decrement leaves the variable unchanged';

class NestedStep {
    method succ { NoStep.new.succ }
    method pred { NoStep.new.pred }
}
my $nested = NestedStep.new;
throws-like { $nested++ }, X::Method::NotFound, message => / 'NoStep' /,
    'a missing succ raised inside the user method propagates';
throws-like { $nested-- }, X::Method::NotFound, message => / 'NoStep' /,
    'a missing pred raised inside the user method propagates';

my $date = Date.new('2024-02-28');
$date++;
is $date.Str, '2024-02-29', 'native instance succ still steps a Date';
$date--;
is $date.Str, '2024-02-28', 'native instance pred still steps a Date';
done-testing;
