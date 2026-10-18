use Test;

plan 4;

class ExportedNamed {
    method go($class = $?CLASS: Str:D :$css!, Bool :$html, Bool :$xml = !$html) is export {
        "{$class.^name} $css {$xml ?? 'xml' !! 'html'}"
    }
}

import ExportedNamed;

is go(:css<a>), 'ExportedNamed a xml', 'the sub form takes the invocant default from the declaring class and forwards named args';
is go(:css<b>, :html), 'ExportedNamed b html', 'a given named flag is forwarded';
is ExportedNamed.go(:css<c>), 'ExportedNamed c xml', 'the method form is unchanged';
dies-ok { go() }, 'a missing required named argument still dies';
