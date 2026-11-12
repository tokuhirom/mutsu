unit module ReqScopeBase;
class Base { method origin { 'own class' } }
class Kid is Base { }
our sub origin() { Kid.origin }
