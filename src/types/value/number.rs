use crate::error::ArgumentError;
use crate::constants::types::{LUA_INT, LUA_FLOAT};
use std::ops::{Add, Sub, Mul, Div, Rem, BitAnd, BitOr, BitXor, Shl, Shr, Neg};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::fmt;
use std::convert::TryInto;
use crate::types::{LuaType, CoerceFrom};
use crate::types::value::LuaValue;

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum LuaNumber {
    INT(LUA_INT),
    FLOAT(LUA_FLOAT),
}

impl LuaNumber {
    pub fn  as_int(&self) -> Result<LUA_INT, ArgumentError> {    // TODO: This silently floors when integer-only is probably more expected
        match self {
            LuaNumber::INT(int) => Ok(*int),
            LuaNumber::FLOAT(float) => {
                if float.is_finite() {
                    Ok(*float as LUA_INT)
                } else {
                    Err(ArgumentError::InvalidType { expected: "Integer or finite float", found: "Infinite float" })
                }
            }
        }
    }

    pub fn as_float(&self) -> LUA_FLOAT {
        match self {
            LuaNumber::INT(int) => *int as LUA_FLOAT,
            LuaNumber::FLOAT(float) => *float,
        }
    }

    pub fn binary_op(lhs: LuaNumber, rhs: LuaNumber, intfunc: fn(LUA_INT, LUA_INT) -> Option<LUA_INT>, floatfunc: fn(LUA_FLOAT, LUA_FLOAT) -> LUA_FLOAT) -> LuaNumber {
        match (lhs, rhs) {
            (LuaNumber::INT(l_i), LuaNumber::INT(r_i)) => {
                intfunc(l_i, r_i).map(LuaNumber::from)
                    .unwrap_or_else(|| { LuaNumber::from(floatfunc(lhs.as_float(), rhs.as_float())) })
            }
            _ => {
                LuaNumber::from(floatfunc(lhs.as_float(), rhs.as_float()))
            }
        }
    }

    pub fn unary_op(value: LuaNumber, intfunc: fn(LUA_INT) -> LUA_INT, floatfunc: fn(LUA_FLOAT) -> LUA_FLOAT) -> LuaNumber {
        match value {
            LuaNumber::INT(int) => LuaNumber::INT(intfunc(int)),
            LuaNumber::FLOAT(float) => LuaNumber::FLOAT(floatfunc(float)),
        }
    }

    pub fn floor(&self) -> Self {
        match self {
            LuaNumber::INT(_) => *self,
            LuaNumber::FLOAT(f) => { LuaNumber::from(f.floor()) }
        }
    }
}

impl LuaType for LuaNumber {
    const CONTAINER_NAME: &'static str = "number";
}

impl<T: Into<LuaValue> + Clone> CoerceFrom<T> for LuaNumber {
    fn coerce(value: &T) -> Option<Self> {
        match value.clone().into() {
            LuaValue::NUMBER(number) => Some(number.clone()),
            LuaValue::STRING(string) => {
                string.try_utf8().ok().and_then(|string| {
                    string.parse::<LUA_INT>().map(LuaNumber::from).or_else(
                        |_| string.parse::<LUA_FLOAT>().map(LuaNumber::from)
                    ).ok()
                })
            }
            _ => None
        }
    }
}

impl From<LUA_INT> for LuaNumber {
    fn from(int: LUA_INT) -> Self {
        LuaNumber::INT(int)
    }
}

impl From<LUA_FLOAT> for LuaNumber {
    fn from(float: LUA_FLOAT) -> Self {
        LuaNumber::FLOAT(float)
    }
}

impl From<usize> for LuaNumber {
    fn from(num: usize) -> Self {
        match num.try_into() {  // TODO: This is dumb, fix it.
            Ok(n) => LuaNumber::INT(n),
            Err(_) => LuaNumber::FLOAT(num as f64)
        }
    }
}

macro_rules! delegate_math_to_primitive {
    ($trait:ident, $func:ident, $int_func:ident, $float_op:tt) => {
        impl $trait for LuaNumber {
            type Output = Result<LuaNumber, ArgumentError>;

            fn $func(self, rhs: Self) -> Self::Output {
                Ok(LuaNumber::binary_op(self, rhs, |lhs, rhs| {lhs.$int_func(rhs)}, |lhs, rhs| {lhs $float_op rhs}))
            }
        }
    };
($trait:ident, $func:ident, intonly: $float_op:tt) => {
impl $trait for LuaNumber {
    type Output = Result<LuaNumber, ArgumentError>;

    fn $func(self, rhs: Self) -> Self::Output {
        Ok(LuaNumber::INT(self.as_int()? $float_op rhs.as_int()?))
    }
}
};
($trait:ident, $func:ident, unary: $op:tt) => {
impl $trait for LuaNumber {
    type Output = Result<LuaNumber, ArgumentError>;

    fn $func(self) -> Self::Output {
        Ok(LuaNumber::unary_op(self, |i| {$op i}, |f| {$op f}))
    }
}
};
}

delegate_math_to_primitive!(Add, add, checked_add, +);
delegate_math_to_primitive!(Sub, sub, checked_sub, -);
delegate_math_to_primitive!(Mul, mul, checked_mul, *);
delegate_math_to_primitive!(Div, div, checked_div, /);
delegate_math_to_primitive!(Rem, rem, checked_rem, %);
delegate_math_to_primitive!(BitAnd, bitand, intonly: &);
delegate_math_to_primitive!(BitOr, bitor, intonly: |);
delegate_math_to_primitive!(BitXor, bitxor, intonly: ^);
delegate_math_to_primitive!(Shl, shl, intonly: <<);
delegate_math_to_primitive!(Shr, shr, intonly: >>);
delegate_math_to_primitive!(Neg, neg, unary: -);

impl LuaNumber {
    pub fn pow(self, rhs: Self) -> Result<LuaNumber, ArgumentError> {
        Ok(LuaNumber::binary_op(
            self, rhs,
            |lhs, rhs| {
                rhs.try_into().ok().and_then(|rhs| { lhs.checked_pow(rhs) })
            },
            |lhs, rhs| { lhs.powf(rhs) }))
    }

    pub fn idiv(self, rhs: Self) -> Result<LuaNumber, ArgumentError> {
        Ok((self / rhs)?.floor())
    }

    pub fn bnot(self) -> Result<LuaNumber, ArgumentError> {
        Ok(LuaNumber::INT(!self.as_int()?))
    }
}

impl PartialOrd for LuaNumber {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            LuaNumber::INT(lhs) => {
                match other {
                    LuaNumber::INT(rhs) => {
                        lhs.partial_cmp(rhs)
                    }
                    LuaNumber::FLOAT(rhs) => {
                        (*lhs as f64).partial_cmp(rhs)
                    }
                }
            }
            LuaNumber::FLOAT(lhs) => {
                match other {
                    LuaNumber::INT(rhs) => {
                        lhs.partial_cmp(&(*rhs as f64))
                    }
                    LuaNumber::FLOAT(rhs) => {
                        lhs.partial_cmp(rhs)
                    }
                }
            }
        }
    }
}


impl Display for LuaNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            LuaNumber::INT(integer) => write!(f, "{}", integer),
            LuaNumber::FLOAT(float) => write!(f, "{}", float),
        }
    }
}
