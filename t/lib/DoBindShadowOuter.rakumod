unit module DoBindShadowOuter;
need DoBindShadowInner;

our sub shadow-do($a) is export { "local($a)" }
sub call-local() is export { shadow-do(1) }
sub call-inner() is export { DoBindShadowInner::shadow-do(1, 2, 3) }
