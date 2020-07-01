use std::rc::Rc;
use crate::error::ArgumentError;
use std::fmt::{Display, Formatter};
use std::fmt;
use std::cmp::Ordering;
use crate::types::{LuaType, CoerceFrom};
use crate::types::value::LuaValue;
use crate::types::value::number::LuaNumber;

#[derive(Clone, Debug)]
pub enum LuaString {
    UNICODE(Rc<str>),
    BINARY(Rc<[u8]>),
}

impl LuaType for LuaString {
    const CONTAINER_NAME: &'static str = "string";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaString {
    fn coerce(value: &T) -> Option<Self> {
        match value.clone().into() {
            LuaValue::STRING(string) => Some(string),
            LuaValue::NUMBER(number) => {
                let string = match number {
                    LuaNumber::INT(int) => int.to_string(),
                    LuaNumber::FLOAT(float) => float.to_string(),
                };
                Some(LuaString::from(string))
            }
            _ => None
        }
    }
}

impl From<&str> for LuaString {
    fn from(string: &str) -> Self {
        LuaString::UNICODE(Rc::from(string))
    }
}

impl From<String> for LuaString {
    fn from(string: String) -> Self {
        LuaString::UNICODE(Rc::from(string))
    }
}

impl From<&[u8]> for LuaString {
    fn from(string: &[u8]) -> Self {
        match std::str::from_utf8(string) {
            Ok(s) => LuaString::UNICODE(Rc::from(s)),
            Err(_) => LuaString::BINARY(Rc::from(string)),
        }
    }
}

impl From<Box<[u8]>> for LuaString {
    fn from(boxed: Box<[u8]>) -> Self {
        match std::str::from_utf8(&boxed) {
            Ok(string) => unsafe {
                LuaString::UNICODE(Rc::from(std::str::from_boxed_utf8_unchecked(boxed)))
            },
            Err(_) => {
                LuaString::BINARY(Rc::from(boxed))
            }
        }
    }
}

impl LuaString {
    pub fn as_bytes(&self) -> &[u8] {
        match self {
            LuaString::UNICODE(string) => string.as_bytes(),
            LuaString::BINARY(bytes) => bytes,
        }
    }

    pub fn append(self, other: &LuaString) -> LuaString {
        match self {
            LuaString::UNICODE(self_string) => {
                match other {
                    LuaString::UNICODE(other_string) => {
                        let mut new_string = String::from(&*self_string);
                        new_string.push_str(&*other_string);
                        LuaString::UNICODE(Rc::from(new_string))
                    }
                    LuaString::BINARY(other_bytes) => {
                        let mut vec = Vec::from(self_string.as_bytes());
                        vec.extend_from_slice(&*other_bytes);
                        LuaString::BINARY(Rc::from(vec))
                    }
                }
            }
            LuaString::BINARY(self_bytes) => {
                let mut vec = Vec::from(&*self_bytes);
                vec.extend_from_slice(other.as_bytes());
                LuaString::BINARY(Rc::from(vec))
            }
        }
    }

    pub fn try_utf8(&self) -> Result<&str, ArgumentError> {
        match self {
            LuaString::UNICODE(unicode) => Ok(unicode),
            LuaString::BINARY(bytes) => {
                debug_assert!(std::str::from_utf8(bytes).is_err());
                Err(ArgumentError::InvalidType { expected: "unicode string", found: "binary string" })
            }
        }
    }

    pub fn len(&self) -> usize {
        match self {
            LuaString::UNICODE(s) => s.len(),
            LuaString::BINARY(s) => s.len(),
        }
    }
}

impl AsRef<[u8]> for LuaString {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl Display for LuaString {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            LuaString::UNICODE(string) => {
                write!(f, "{}", *string)
            }
            LuaString::BINARY(bytes) => {
                write!(f, "{:X?}", bytes)
            }
        }
    }
}

impl PartialEq for LuaString {
    fn eq(&self, other: &Self) -> bool {
        self.as_bytes() == other.as_bytes()
    }
}

impl PartialEq<str> for LuaString {
    fn eq(&self, other: &str) -> bool {
        match self {
            LuaString::UNICODE(u) => u.as_ref() == other,
            LuaString::BINARY(b) => b.as_ref() == other.as_bytes(),
        }
    }
}

impl PartialOrd for LuaString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_bytes().partial_cmp(&other.as_bytes())
    }
}
