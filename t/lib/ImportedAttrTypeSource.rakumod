unit module ImportedAttrTypeSource;

class LineParser is export {
    has Str $!buffer = '';
    method feed(Str $chunk) { $!buffer ~= $chunk; $!buffer }
}
