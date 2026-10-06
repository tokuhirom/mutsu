use SwjKl;
use SwjItf;
class SwjHolder is export {
    my subset Unit where SwjKl|SwjItf;
    has Unit $.type;
}
