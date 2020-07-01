use crate::types::value::LuaValue;
use std::ops::{Bound, RangeBounds};

pub struct Varargs { inner: Vec<LuaValue> } // TODO: See if this can be replaced with a boxed slice, but it doesn't really matter.

impl Varargs {
    pub fn nil() -> Varargs {
        Varargs { inner: vec![LuaValue::NIL] }
    }

    pub fn empty() -> Varargs {
        Varargs { inner: vec![] }
    }

    pub fn first(&self) -> &LuaValue {
        self.inner.get(0).unwrap_or(&LuaValue::NIL)
    }

    pub fn into_first(mut self) -> LuaValue {
        self.inner.drain(..).next().unwrap_or(LuaValue::NIL)
    }

    pub fn count(&self) -> usize {
        self.inner.len()
    }

    pub fn n(&self, i: usize) -> &LuaValue {
        self.inner.get(i).unwrap_or(&LuaValue::NIL)
    }

    pub fn opt(&self, i: usize) -> Option<&LuaValue> {
        self.inner.get(i)
    }

    pub fn select_range<T: RangeBounds<usize>>(&self, range: T) -> Varargs {
        macro_rules! slice_end_bound {
        ($skipwhile:expr, $range:expr) => {
            match $range.end_bound() {
                Bound::Included(i) => $skipwhile.take_while(|kv| kv.0 <= *i).map(|kv| kv.1.clone()).collect(),
                Bound::Excluded(i) => $skipwhile.take_while(|kv| kv.0 < *i).map(|kv| kv.1.clone()).collect(),
                Bound::Unbounded => $skipwhile.take_while(|_| true).map(|kv| kv.1.clone()).collect(),
            }
        };
    }

        //noinspection RsLiveness           Used in macro below
        let iter = self.inner.iter().enumerate();
        let inner = match range.start_bound() {
            Bound::Included(i) => slice_end_bound!(iter.skip_while(|kv| kv.0 != *i), range),
            Bound::Excluded(i) => slice_end_bound!(iter.skip_while(|kv| kv.0 != (i + 1)), range),
            Bound::Unbounded => slice_end_bound!(iter.skip_while(|kv| kv.1 == &LuaValue::NIL), range),
        };
        Varargs { inner }
    }

    pub fn as_slice(&self) -> &[LuaValue] {
        &self.inner[..]
    }

    // pub fn checked_slice<T: RangeBounds<usize>>(&self, range: T) -> &[LuaValue] {   // TODO: write test
    //     match range.start_bound() {
    //         Bound::Included(i) if i < &self.inner.len() => return &[],
    //         Bound::Excluded(i) if i < &self.inner.len().saturating_sub(1) => return &[],
    //         _ => {}
    //     }
    //     match range.end_bound() {
    //         Bound::Included(i) if i >= &self.inner.len() => return &[],
    //         Bound::Excluded(i) if i > &self.inner.len() => return &[],
    //         _ => {}
    //     }
    //
    //     match range.start_bound() {
    //         Bound::Included(start) => match range.end_bound() {
    //             Bound::Included(end) => &self.inner[*start..=*end],
    //             Bound::Excluded(end) => &self.inner[*start..*end],
    //             Bound::Unbounded => &self.inner[*start..]
    //         },
    //         Bound::Excluded(start) => {
    //             let start = start.saturating_add(1);
    //             match range.end_bound() {
    //                 Bound::Included(end) => &self.inner[start..=*end],
    //                 Bound::Excluded(end) => &self.inner[start..*end],
    //                 Bound::Unbounded => &self.inner[start..]
    //             }
    //         }
    //         Bound::Unbounded => match range.end_bound() {
    //             Bound::Included(end) => &self.inner[..=*end],
    //             Bound::Excluded(end) => &self.inner[..*end],
    //             Bound::Unbounded => &self.inner[..]
    //         },
    //     }
    // }

    pub fn prepend<T: Into<Varargs>>(prefix: T, body: &Self) -> Varargs {
        let mut new_varargs = prefix.into();
        new_varargs.inner.extend_from_slice(&body.inner[..]);
        new_varargs
    }
}

impl<T: Into<LuaValue>> From<T> for Varargs {
    fn from(val: T) -> Self {
        Varargs {
            inner: vec![val.into()]
        }
    }
}

impl From<&[LuaValue]> for Varargs {
    fn from(slice: &[LuaValue]) -> Self {
        Varargs { inner: Vec::from(slice) }
    }
}

impl From<Vec<LuaValue>> for Varargs {
    fn from(inner: Vec<LuaValue>) -> Self {
        Varargs { inner }
    }
}

impl<T: Into<LuaValue>, U: Into<LuaValue>> From<(T, U)> for Varargs {
    fn from(tuple: (T, U)) -> Self {
        Varargs { inner: vec![tuple.0.into(), tuple.1.into()] }
    }
}

impl<T: Into<LuaValue>, U: Into<LuaValue>, V: Into<LuaValue>> From<(T, U, V)> for Varargs {
    fn from(tuple: (T, U, V)) -> Self {
        Varargs { inner: vec![tuple.0.into(), tuple.1.into(), tuple.2.into()] }
    }
}
