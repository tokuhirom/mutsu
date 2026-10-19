unit module UseInBlockCoreOpLexical;

# User candidates of core operators whose op asks no user candidate, for
# t/modules/import-export/use-in-block-binds-core-operators-lexically.t
class Cop is export { }
multi sub infix:<%%>(Cop $a, Cop $b) is export { 'C:%%' }
multi sub infix:<+&>(Cop $a, Cop $b) is export { 'C:+&' }
multi sub infix:«+<»(Cop $a, Cop $b) is export { 'C:+<' }
multi sub infix:<~^>(Cop $a, Cop $b) is export { 'C:~^' }
multi sub infix:<~|>(Cop $a, Cop $b) is export { 'C:~|' }
multi sub infix:<?|>(Cop $a, Cop $b) is export { 'C:?|' }
multi sub infix:<..>(Cop $a, Cop $b) is export { 'C:..' }
multi sub infix:<^..^>(Cop $a, Cop $b) is export { 'C:^..^' }
multi sub infix:«<=>»(Cop $a, Cop $b) is export { 'C:<=>' }
multi sub infix:<===>(Cop $a, Cop $b) is export { 'C:===' }
multi sub infix:<=~=>(Cop $a, Cop $b) is export { 'C:=~=' }
multi sub infix:«=>»(Cop $a, Cop $b) is export { 'C:=>' }
multi sub infix:<|>(Cop $a, Cop $b) is export { 'C:|' }
multi sub infix:<before>(Cop $a, Cop $b) is export { 'C:before' }
multi sub infix:<but>(Cop $a, Cop $b) is export { 'C:but' }
multi sub infix:<unicmp>(Cop $a, Cop $b) is export { 'C:unicmp' }
multi sub infix:<minmax>(Cop $a, Cop $b) is export { 'C:minmax' }
multi sub prefix:<^>(Cop $a) is export { 'C:^' }
multi sub prefix:<+^>(Cop $a) is export { 'C:+^' }
multi sub prefix:<?^>(Cop $a) is export { 'C:?^' }
