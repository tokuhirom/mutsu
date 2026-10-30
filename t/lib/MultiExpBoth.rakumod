use MultiExpT;
use MultiExpIO;
sub EXPORT {
    %(
        MultiExpT::EXPORT::DEFAULT::,
        MultiExpIO::EXPORT::DEFAULT::,
    )
}
unit module MultiExpBoth;
