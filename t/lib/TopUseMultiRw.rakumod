unit module TopUseMultiRw;
multi sub bump(Int $x is rw) is export { $x++ }
multi sub bump(Str $x is rw) is export { $x ~= '!' }
multi sub peek(\x) is export { x.VAR.^name }
multi sub peek(Str \x, Int) is export { 'two' }
