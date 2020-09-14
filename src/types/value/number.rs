use crate::error::ArgumentError;
use crate::constants::types::{LUA_INT, LUA_INT_UNSIGNED, LUA_FLOAT};
use std::ops::{Add, Sub, Mul, Div, Rem, BitAnd, BitOr, BitXor, Shl, Shr, Neg};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::fmt;
use std::convert::TryInto;
use crate::types::{LuaType, CoerceFrom};
use crate::types::value::LuaValue;

#[derive(Copy, Clone, Debug)]
pub enum LuaNumber {
    INT(LUA_INT),
    FLOAT(LUA_FLOAT),
}

impl LuaNumber {
    pub fn as_int(&self) -> Result<LUA_INT, ArgumentError> {    // TODO: This silently floors when integer-only is probably more expected
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

    pub fn binary_op(lhs: LuaNumber, rhs: LuaNumber, intfunc: fn(LUA_INT, LUA_INT) -> LUA_INT, floatfunc: fn(LUA_FLOAT, LUA_FLOAT) -> LUA_FLOAT) -> LuaNumber {
        match (lhs, rhs) {
            (LuaNumber::INT(l_i), LuaNumber::INT(r_i)) => {
                LuaNumber::from(intfunc(l_i, r_i))
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
                if let Some(utf8) = string.try_utf8().ok() {
                    let utf8 = utf8.trim();
                    if let Ok(int) = utf8.parse::<LUA_INT>() {
                        Some(LuaNumber::from(int))
                    } else if let Ok(float) = utf8.parse::<LUA_FLOAT>() {
                        Some(LuaNumber::from(float))
                    } else {
                        let mut negate = false;
                        let utf8 = if let Some(utf8) = utf8.strip_prefix("-") {
                            negate = true;
                            utf8
                        } else {
                            utf8
                        };
                        let utf8 = utf8.strip_prefix("0x").unwrap_or(utf8);
                        if let Ok(unsigned) = LUA_INT_UNSIGNED::from_str_radix(utf8, 16) {
                            if negate {
                                Some(LuaNumber::from(-(unsigned as i64)))
                            } else {
                                Some(LuaNumber::from(unsigned as i64))
                            }
                        } else {
                            None
                        }
                    }
                } else {
                    None
                }
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
    ($trait:ident, $func:ident, floatonly: $float_op:tt) => {
        impl $trait for LuaNumber {
            type Output = Result<LuaNumber, ArgumentError>;

            fn $func(self, rhs: Self) -> Self::Output {
                Ok(LuaNumber::FLOAT(self.as_float() $float_op rhs.as_float()))
            }
        }
    };
    ($trait:ident, $func:ident, intonly: $int_op:tt) => {
        impl $trait for LuaNumber {
            type Output = Result<LuaNumber, ArgumentError>;

            fn $func(self, rhs: Self) -> Self::Output {
                Ok(LuaNumber::INT(self.as_int()? $int_op rhs.as_int()?))
            }
        }
    };
    ($trait:ident, $func:ident, uintonly: $int_op:tt) => {
        impl $trait for LuaNumber {
            type Output = Result<LuaNumber, ArgumentError>;

            fn $func(self, rhs: Self) -> Self::Output {
                Ok(LuaNumber::INT(((self.as_int()? as LUA_INT_UNSIGNED) $int_op (rhs.as_int()? as LUA_INT_UNSIGNED)) as LUA_INT))
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

delegate_math_to_primitive!(Add, add, wrapping_add, +);
delegate_math_to_primitive!(Sub, sub, wrapping_sub, -);
delegate_math_to_primitive!(Mul, mul, wrapping_mul, *);
delegate_math_to_primitive!(Div, div, floatonly: /); // TODO: Fix div-by-zero
delegate_math_to_primitive!(Rem, rem, wrapping_rem, %);
delegate_math_to_primitive!(BitAnd, bitand, intonly: &);
delegate_math_to_primitive!(BitOr, bitor, intonly: |);
delegate_math_to_primitive!(BitXor, bitxor, intonly: ^);
// delegate_math_to_primitive!(Shl, shl, uintonly: <<);
impl Shl for LuaNumber {
    type Output = Result<LuaNumber, ArgumentError>;

    fn shl(self, rhs: Self) -> Self::Output {
        if rhs < 0 {
            self.shr((-rhs)?)
        } else {
            let (value, did_overflow) = (self.as_int()? as LUA_INT_UNSIGNED).overflowing_shl(rhs.as_int()? as u32);
            Ok(LuaNumber::INT(if did_overflow { 0 } else { value } as LUA_INT))
        }
    }
}
// delegate_math_to_primitive!(Shr, shr, uintonly: >>);
impl Shr for LuaNumber {
    type Output = Result<LuaNumber, ArgumentError>;

    fn shr(self, rhs: Self) -> Self::Output {
        if rhs < 0 {
            self.shl((-rhs)?)
        } else {
            let (value, did_overflow) = (self.as_int()? as LUA_INT_UNSIGNED).overflowing_shr(rhs.as_int()? as u32);
            Ok(LuaNumber::INT(if did_overflow { 0 } else { value } as LUA_INT))
        }
    }
}
delegate_math_to_primitive!(Neg, neg, unary: -);

impl LuaNumber {
    pub fn pow(self, rhs: Self) -> Result<LuaNumber, ArgumentError> {
        Ok(LuaNumber::from(self.as_float().powf(rhs.as_float())))
    }

    pub fn idiv(self, rhs: Self) -> Result<LuaNumber, ArgumentError> {
        Ok((self / rhs)?.floor())
    }

    pub fn bnot(self) -> Result<LuaNumber, ArgumentError> {
        Ok(LuaNumber::INT(!self.as_int()?))
    }
}

impl PartialEq for LuaNumber {
    fn eq(&self, other: &Self) -> bool {
        match self {
            LuaNumber::INT(lhs) => {
                match other {
                    LuaNumber::INT(rhs) => lhs.eq(rhs),
                    LuaNumber::FLOAT(rhs) => {
                        if (*rhs as LUA_INT) as LUA_FLOAT == *rhs {
                            lhs.eq(&(*rhs as LUA_INT))
                        } else {
                            false
                        }
                    },
                }
            },
            LuaNumber::FLOAT(lhs) => {
                match other {
                    LuaNumber::INT(rhs) => lhs.eq(&(*rhs as LUA_FLOAT)),
                    LuaNumber::FLOAT(rhs) => lhs.eq(rhs),
                }
            },
        }
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

impl PartialEq<LUA_INT> for LuaNumber {
    fn eq(&self, other: &i64) -> bool {
        match self {
            LuaNumber::INT(i) => i == other,
            LuaNumber::FLOAT(f) => *f as LUA_INT == *other && *f == ((*f as LUA_INT) as LUA_FLOAT),
        }
    }
}

impl PartialOrd<LUA_INT> for LuaNumber {
    fn partial_cmp(&self, rhs: &LUA_INT) -> Option<Ordering> {
        match self {
            LuaNumber::INT(lhs) => {
                lhs.partial_cmp(rhs)
            }
            LuaNumber::FLOAT(lhs) => {
                lhs.partial_cmp(&(*rhs as f64))
            }
        }
    }
}

impl PartialEq<LUA_FLOAT> for LuaNumber {
    fn eq(&self, other: &f64) -> bool {
        match self {
            LuaNumber::INT(i) => *i as LUA_FLOAT == *other && *i == ((*i as LUA_FLOAT) as LUA_INT),
            LuaNumber::FLOAT(f) => f == other,
        }
    }
}


impl PartialOrd<LUA_FLOAT> for LuaNumber {
    fn partial_cmp(&self, rhs: &LUA_FLOAT) -> Option<Ordering> {
        match self {
            LuaNumber::INT(lhs) => {
                (*lhs as f64).partial_cmp(rhs)
            }
            LuaNumber::FLOAT(lhs) => {
                lhs.partial_cmp(rhs)
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
