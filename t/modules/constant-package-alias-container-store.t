use Test;

plan 9;

module M::N { our @list = 1; our %map; }
constant Al = M::N;

@Al::list[2] = 9;
is @M::N::list.raku, '[1, Any, 9]', 'an indexed store reaches the declared array';
is @Al::list.raku, '[1, Any, 9]', 'the alias reads back the indexed store';

@Al::list = 7, 8;
is @M::N::list.raku, '[7, 8]', 'a whole-array store reaches the declared array';

%Al::map<k> = 'v';
is %M::N::map<k>, 'v', 'an element store reaches the declared hash';
%Al::map = x => 3;
is %M::N::map.raku, '{:x(3)}', 'a whole-hash store reaches the declared hash';

@Al::missing = 3, 4;
is @M::N::missing.raku, '$(3, 4)', 'an undeclared array slot is itemized under the real package';
%Al::other<k> = 5;
is %M::N::other<k>, 5, 'an undeclared hash element vivifies under the real package';

{
    my constant Local = M::N;
    @Local::list[0] = 6;
    is @M::N::list.raku, '[6, 8]', 'a lexical alias also reaches the real array';
    %Local::map<z> = 4;
    is %M::N::map<z>, 4, 'a lexical alias also reaches the real hash';
}
