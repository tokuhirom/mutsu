unit module LooserComma;
sub infix:<graf>($a, +@b) is looser<,> is export { "graf($a; {@b.join(',')})" }
