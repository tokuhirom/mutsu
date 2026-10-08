use Test;
use lib 't/lib';
use FrameEnvCodeBindings;

plan 5;

is FrameEnvCodeBindings::from-module(), 42, 'module routine sees the initial our &answer';
is &FrameEnvCodeBindings::answer(), 42, 'qualified read sees the initial value';

&FrameEnvCodeBindings::answer = { 43 };
is &FrameEnvCodeBindings::answer(), 43, 'qualified write is visible through the qualified name';
is FrameEnvCodeBindings::from-module(), 43, 'qualified write is visible to the module bare &answer';

await start { &FrameEnvCodeBindings::answer = { 44 } };
is FrameEnvCodeBindings::from-module(), 44, 'qualified write from a thread reaches the module alias';
