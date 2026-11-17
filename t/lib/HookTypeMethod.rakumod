use HookTypeMethodBase;
class HookTypeExporter {
    method exports { %( HookTypeMethodBase::EXPORT::ALL:: ) }
}
sub EXPORT() { HookTypeExporter.exports }
