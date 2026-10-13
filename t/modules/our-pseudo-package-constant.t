use Test;

plan 4;

constant c = 1;
is OUR::c, 1, 'OUR::c names the unit-level constant';
is OUR::c + 1, 2, 'usable in an expression';

package M {
    our constant k = 7;
    is OUR::k, 7, 'OUR::k inside a package';
}
is M::k, 7, 'M::k from outside';
