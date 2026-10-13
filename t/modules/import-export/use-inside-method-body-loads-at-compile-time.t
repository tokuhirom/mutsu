use lib 't/lib';
use Test;

# From CSS::Module: CSS::Module::CSS3::Fonts::AtFontFace uses
# CSS::Module::CSS3::Actions only inside a method body, and the classes that
# module registers (CSS::Module::CSS3::Fonts::Actions) are used elsewhere without
# any method having run. `use` is a BEGIN-time load wherever it stands.

plan 2;

use MethodBodyUseHolder;

is %*ENV<MUTSU_METHOD_BODY_USE_LATE>, 'loaded',
    'a module used only inside a method body is loaded before any method runs';
is MethodBodyUseHolder.new.late, 'late-marker', 'the method itself still works';
