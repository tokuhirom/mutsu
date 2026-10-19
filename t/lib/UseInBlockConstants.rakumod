unit module UseInBlockConstants;

constant UIBC-TERM is export = 'module-term';
constant $uibc-sig is export = 'module-sig';
my constant UIBC-MY is export(:my) = 'module-my';
class UibcType is export { method who { 'module type' } }
