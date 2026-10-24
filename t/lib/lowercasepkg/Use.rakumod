use lowercasepkg::Types;
unit module lowercasepkg::Use;
sub ev(Int $d --> Fact) is export {
    return unknown('deep') if $d > 64;
    return unknown('ok');
}
