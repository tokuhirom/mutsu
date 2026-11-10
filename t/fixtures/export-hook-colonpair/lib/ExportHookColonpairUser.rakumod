use ExportHookColonpair;

class ExportHookColonpairUser {
    method via-method(Mu $d) { ExportHookColonpair::render($d, :pretty(True)) }
}
sub via-sub(Mu $d) is export { ExportHookColonpair::render($d, :pretty(True)) }
