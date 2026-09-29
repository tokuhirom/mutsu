//! wasm32 has no pool workers and therefore no yield points: the cooperative
//! pump is already sequential, so a resolution never has a keeper to order a
//! late `await` behind (ADR-0105 D2).

/// Never constructed on wasm32; see the native `yield_points::KeeperMark`.
#[derive(Clone)]
pub(crate) struct KeeperMark;

impl KeeperMark {
    pub(crate) fn defer(
        &self,
        f: Box<dyn FnOnce() + Send + 'static>,
    ) -> Result<(), Box<dyn FnOnce() + Send + 'static>> {
        Err(f)
    }
}

/// No thread is ever a pool worker here.
// Cost: O(1).
pub(crate) fn keeper_mark() -> Option<KeeperMark> {
    None
}
