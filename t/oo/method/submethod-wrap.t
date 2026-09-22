use Test;

plan 4;

my $log = '';
class WrappedSubmethod {
    submethod TWEAK(|) { $log ~= 'original' }
}

my $tweak = WrappedSubmethod.^find_method('TWEAK');
is $tweak.^name, 'Submethod', '.^find_method returns a Submethod object';

my $handle = $tweak.wrap(-> |args {
    $log ~= 'wrapper';
    callsame;
});
ok $handle.defined, '.wrap returns a handle for a Submethod';

WrappedSubmethod.new;
is $log, 'wrapperoriginal', 'the wrapper runs before the Submethod';

$handle.restore;
$log = '';
WrappedSubmethod.new;
is $log, 'original', '.restore removes the Submethod wrapper';
