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

/// 2-type union with no greater meaning to each variant
pub enum Union2<T, U> {
    One(T),
    Two(U)
}

impl<T, U> Union2<T, U> {
    pub fn first(val: T) -> Union2<T, U> {
        Union2::One(val)
    }

    pub fn second(val: U) -> Union2<T, U> {
        Union2::Two(val)
    }
}

/// 3-type union with no greater meaning to each variant
pub enum Union3<T, U, V> {
    One(T),
    Two(U),
    Three(V)
}

pub trait ResultFrom<T>: Sized {
    fn ok_from<E>(val: T) -> Result<Self, E>;
    fn err_from<O>(val: T) -> Result<O, Self>;
}

impl<T, U> ResultFrom<U> for T where T: From<U> {
    fn ok_from<E>(val: U) -> Result<T, E> {
        Result::Ok(T::from(val))
    }

    fn err_from<O>(val: U) -> Result<O, T> {
        Result::Err(T::from(val))
    }
}