pub mod types;
pub mod lambda;
pub use arc_gc::gc::GC as GC;
pub use arc_gc::traceable::GCTraceable as GCTraceable;
pub use arc_gc::arc::GCArc as GCArc;
pub use arc_gc::arc::GCArcWeak as GCArcWeak;
pub mod utils;