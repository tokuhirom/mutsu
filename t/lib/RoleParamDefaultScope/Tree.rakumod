unit package RoleParamDefaultScope;
use RoleParamDefaultScope::Render;
class Basic does Renderer { }
role Tree[::V = Any, Renderer :$gist = Pretty, Renderer :$str = Basic] {
    method kinds { V.^name ~ ' ' ~ $gist.^name ~ ' ' ~ $str.^name }
}
