use std::cell::RefCell;

/// Cell variant that mimics `Cell` behavior with `Clone` instead of `Copy`
#[derive(Debug)]
pub struct CloneCell<T: Clone> {
    inner: RefCell<T>
}

impl<T: Clone> CloneCell<T> {
    pub fn get(&self) -> T {
        self.inner.borrow().clone()
    }

    pub fn replace(&self, value: T) -> T {
        std::mem::replace(&mut *self.inner.borrow_mut(), value)
    }
}

impl<T: Clone> From<T> for CloneCell<T> {
    fn from(val: T) -> Self {
        CloneCell {
            inner: RefCell::from(val)
        }
    }
}

// /// 2-type union with no greater meaning to each variant
// pub enum Union2<T, U> {
//     One(T),
//     Two(U)
// }

/// 3-type union with no greater meaning to each variant
pub enum Union3<T, U, V> {
    One(T),
    Two(U),
    Three(V)
}