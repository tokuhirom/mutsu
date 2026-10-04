use Test;
use lib 't/lib';

plan 8;

sub load-in-sub() {
    require FrameEnvConstants;
}

load-in-sub();
use FrameEnvConstants;

is FrameEnvConstants::VERSION, 17, 'a qualified constant survives its loading frame';
is ::('FrameEnvConstants::VERSION'), 17, 'indirect lookup finds the constant';
is @FrameEnvConstants::VALUES.join(','), '2,3,5', 'a qualified List constant keeps its value';
is %FrameEnvConstants::LABELS<first>, 'one', 'a qualified Map constant keeps its value';
is FrameEnvConstants::own-version(), 17, 'the module reads its own scalar constant';
is FrameEnvConstants::own-values(), '2,3,5', 'the module reads its own List constant';
is FrameEnvConstants::own-label(), 'two', 'the module reads its own Map constant';
is FrameEnvConstants::.<VERSION>, 17, 'the package stash exposes the constant';
