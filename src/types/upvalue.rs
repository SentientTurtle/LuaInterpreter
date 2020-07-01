use std::rc::Rc;
use crate::util::CloneCell;
use crate::types::value::LuaValue;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct UpvalueDesc {
    instack: u8,
    idx: u8,
}

impl UpvalueDesc {
    pub fn new(instack: u8, idx: u8) -> Self {
        UpvalueDesc { instack, idx }
    }

    pub fn index(&self) -> usize {
        self.idx as usize
    }

    pub fn in_stack(&self) -> bool {
        self.instack != 0
    }

    pub fn bytes(&self) -> [u8; 2] {
        [self.instack, self.idx]
    }
}

impl Display for UpvalueDesc {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "instack: {}\tidx:{}", self.instack, self.idx)
    }
}

#[derive(Debug, Clone)]
pub enum UpvalueImpl {
    Open { frame: usize, register: usize },
    Closed(LuaValue),
}

#[derive(Debug, Clone)]
pub struct Upvalue { inner: Rc<CloneCell<UpvalueImpl>> }

impl Upvalue {
    pub fn new_open(register: usize, stack_index: usize) -> Upvalue {
        Upvalue {
            inner: Rc::from(CloneCell::from(UpvalueImpl::Open { frame: stack_index, register }))
        }
    }

    pub fn new_closed(value: LuaValue) -> Upvalue {
        Upvalue {
            inner: Rc::new(CloneCell::from(UpvalueImpl::Closed(value)))
        }
    }

    pub fn get(&self) -> UpvalueImpl {
        self.inner.get()
    }

    pub fn close(&self, value: LuaValue) {
        self.inner.replace(UpvalueImpl::Closed(value));
    }
}
