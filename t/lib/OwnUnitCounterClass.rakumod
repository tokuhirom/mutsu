# No `unit module`: the class is declared at the file's top level, so its
# methods run in package GLOBAL but in this file's compilation unit.
use OwnUnitHelperProvider;

class OwnUnitCounter is export {
    method step($s) { counted($s) }
}
