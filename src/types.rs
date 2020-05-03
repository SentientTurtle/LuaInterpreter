use std::rc::Rc;
use std::collections::HashMap;
use std::any::Any;
use crate::constants;
use crate::constants::types::{LUA_INT, LUA_FLOAT, LUA_INSTRUCTION, HOST_INT};
use std::fmt::{Display, Formatter, Error};
use std::cell::RefCell;
use std::ops::*;
use crate::util::CloneCell;
use std::convert::TryInto;
use std::result::Result::Ok;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::mem;
use crate::vm::ExecuteError;

#[derive(Debug)]
pub struct TypeCastError { pub(crate) expected: &'static str, pub(crate) found: &'static str }

#[derive(Debug)]
pub enum TypeCoerceError {
    Cast(TypeCastError),
    StringNotNumerical(LuaString),
}

impl From<TypeCastError> for TypeCoerceError {
    fn from(e: TypeCastError) -> Self {
        TypeCoerceError::Cast(e)
    }
}

#[derive(Debug)]
pub enum MathError {
    CannotConvertNonFiniteFloatToInt,
    CouldNotCast(TypeCastError),
    ConcatOverflow,
    IncomparableTypes(&'static str, &'static str),
}

impl From<TypeCastError> for MathError {
    fn from(t: TypeCastError) -> Self {
        MathError::CouldNotCast(t)
    }
}

pub struct Varargs { inner: Vec<LuaValue> }

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
}

impl From<Vec<LuaValue>> for Varargs {
    fn from(inner: Vec<LuaValue>) -> Self {
        Varargs { inner }
    }
}

#[derive(Clone)]
pub enum LuaValue {
    NIL,
    BOOLEAN(bool),
    NUMBER(LuaNumber),
    STRING(LuaString),
    USERDATA(UserData),
    FUNCTION(LuaFunction),
    THREAD(LuaThread),
    TABLE(LuaTable),
}

impl LuaValue {
    pub fn type_name(&self) -> &'static str {
        match self {
            LuaValue::NIL => "nil",
            LuaValue::BOOLEAN(_) => "boolean",
            LuaValue::NUMBER(_) => "number",
            LuaValue::STRING(_) => "string",
            LuaValue::USERDATA(_) => "userdata",
            LuaValue::FUNCTION(_) => "function",
            LuaValue::THREAD(_) => "thread",
            LuaValue::TABLE(_) => "table",
        }
    }

    pub fn try_number(&self) -> Result<&LuaNumber, TypeCastError> {
        match self {
            LuaValue::NUMBER(number) => Ok(number),
            _ => Err(TypeCastError { expected: "number", found: self.type_name() })
        }
    }

    pub fn coerce_number(&self) -> Result<LuaNumber, TypeCoerceError> {
        match self {
            LuaValue::NUMBER(number) => Ok(number.clone()),
            LuaValue::STRING(string) => {
                let utf8 = string.try_utf8()?;
                match utf8.parse::<LUA_INT>() {
                    Ok(int) => Ok(LuaNumber::INT(int)),
                    Err(_) => {
                        match utf8.parse::<LUA_FLOAT>() {
                            Ok(float) => Ok(LuaNumber::FLOAT(float)),
                            Err(_) => Err(TypeCoerceError::StringNotNumerical(string.clone())),
                        }
                    }
                }
            }
            _ => Err(TypeCoerceError::Cast(TypeCastError { expected: "number", found: self.type_name() }))
        }
    }

    pub fn try_table(&self) -> Result<&LuaTable, TypeCastError> {
        match self {
            LuaValue::TABLE(table) => Ok(table),
            _ => Err(TypeCastError { expected: "table", found: self.type_name() })
        }
    }

    pub fn try_function(&self) -> Result<&LuaFunction, TypeCastError> {
        match self {
            LuaValue::FUNCTION(func) => Ok(func),
            _ => Err(TypeCastError { expected: "function", found: self.type_name() })
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            LuaValue::NIL => false,
            LuaValue::BOOLEAN(b) => *b,
            _ => true
        }
    }
}

impl From<LUA_INT> for LuaValue {
    fn from(i: LUA_INT) -> Self {
        LuaValue::NUMBER(LuaNumber::INT(i))
    }
}

impl From<LUA_FLOAT> for LuaValue {
    fn from(f: LUA_FLOAT) -> Self {
        LuaValue::NUMBER(LuaNumber::FLOAT(f))
    }
}

impl From<&'static str> for LuaValue {
    fn from(string: &'static str) -> Self {
        LuaValue::STRING(LuaString::UNICODE(Rc::from(string)))
    }
}

impl From<bool> for LuaValue {
    fn from(b: bool) -> Self {
        LuaValue::BOOLEAN(b)
    }
}

impl From<LuaNumber> for LuaValue {
    fn from(n: LuaNumber) -> Self {
        LuaValue::NUMBER(n)
    }
}

impl From<LuaString> for LuaValue {
    fn from(s: LuaString) -> Self {
        LuaValue::STRING(s)
    }
}

impl From<UserData> for LuaValue {
    fn from(u: UserData) -> Self {
        LuaValue::USERDATA(u)
    }
}

impl From<LuaFunction> for LuaValue {
    fn from(f: LuaFunction) -> Self {
        LuaValue::FUNCTION(f)
    }
}

// Apparently this doesn't work?
// impl From<fn(&[LuaValue]) -> Result<Varargs, ExecuteError>> for LuaValue {
//     fn from(func: fn(&[LuaValue]) -> Result<Varargs, ExecuteError>) -> Self {
//         LuaValue::FUNCTION(LuaFunction::RUST(func))
//     }
// }

impl From<LuaThread> for LuaValue {
    fn from(t: LuaThread) -> Self {
        LuaValue::THREAD(t)
    }
}

impl From<LuaTable> for LuaValue {
    fn from(t: LuaTable) -> Self {
        LuaValue::TABLE(t)
    }
}

// TODO: Note that these don't invoke metamethods in documentation
macro_rules! delegate_math_to_luanumber {
    ($trait:ident, $func:ident, $op:tt) => {
        impl $trait for &LuaValue {
            type Output = Result<LuaValue, MathError>;
            fn $func(self, rhs: Self) -> Self::Output {
                Ok(LuaValue::from((*self.try_number()? $op *rhs.try_number()?)?))
            }
        }
    };
    ($trait:ident, $func:ident, unary: $op:tt) => {
        impl $trait for &LuaValue {
            type Output = Result<LuaValue, MathError>;
            fn $func(self) -> Self::Output {
                Ok(LuaValue::from(($op *self.try_number()?)?))
            }
        }
    };
    (func: $func:ident) => {
        impl LuaValue {
            pub fn $func(&self, rhs: &Self) -> Result<LuaValue, MathError> {
                Ok(LuaValue::from((*self.try_number()?).$func(*rhs.try_number()?)?))
            }
        }
    };
}

delegate_math_to_luanumber!(Add, add, +);
delegate_math_to_luanumber!(Sub, sub, -);
delegate_math_to_luanumber!(Mul, mul, *);
delegate_math_to_luanumber!(Div, div, /);
delegate_math_to_luanumber!(Rem, rem, %);
delegate_math_to_luanumber!(func: pow);
delegate_math_to_luanumber!(func: idiv);
delegate_math_to_luanumber!(BitAnd, bitand, &);
delegate_math_to_luanumber!(BitOr, bitor, |);
delegate_math_to_luanumber!(BitXor, bitxor, ^);
delegate_math_to_luanumber!(Shl, shl, <<);
delegate_math_to_luanumber!(Shr, shr, >>);
delegate_math_to_luanumber!(Neg, neg, unary: -);

impl LuaValue {
    pub fn bnot(&self) -> Result<LuaValue, MathError> {
        Ok(LuaValue::from(self.try_number()?.bnot()?))
    }
}

impl Not for &LuaValue {
    type Output = Result<LuaValue, MathError>;

    fn not(self) -> Self::Output {
        Ok(LuaValue::from(!self.is_truthy()))
    }
}

impl LuaValue {
    pub fn len(&self) -> Result<LuaValue, MathError> {
        match self {
            LuaValue::STRING(s) => Ok(LuaValue::NUMBER(LuaNumber::from(s.len()))),
            LuaValue::TABLE(t) => Ok(LuaValue::NUMBER(LuaNumber::from(t.len()))),
            n @ _ => Err(MathError::CouldNotCast(TypeCastError { expected: "String or table", found: self.type_name() }))
        }
    }

    pub fn concat(values: &[&LuaValue]) -> Result<LuaValue, MathError> {
        let mut new_len = 0usize;
        for value in values {
            if let LuaValue::STRING(s) = value {
                new_len = (new_len.checked_add(s.len()): Option<usize>).ok_or(MathError::ConcatOverflow)?
            } else {
                return Err(MathError::from(TypeCastError { expected: "string", found: value.type_name() }));
            }
        }
        let mut buffer: Vec<u8> = Vec::with_capacity(new_len); // TODO: Fix potential for huge icky allocations
        for value in values {
            if let LuaValue::STRING(s) = value {
                buffer.extend_from_slice(s.as_bytes());
            } else {
                unreachable!()  // We already checked that each value is a ::STRING in the above loop
            }
        }
        Ok(LuaValue::STRING(LuaString::from(buffer.into_boxed_slice())))
    }
}

impl PartialEq<bool> for LuaValue {
    fn eq(&self, other: &bool) -> bool {
        self.is_truthy() == *other
    }
}

impl PartialEq for LuaValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LuaValue::NIL, LuaValue::NIL) => true,
            (LuaValue::BOOLEAN(lhs), LuaValue::BOOLEAN(rhs)) => lhs == rhs,
            (LuaValue::NUMBER(lhs), LuaValue::NUMBER(rhs)) => lhs == rhs,
            (LuaValue::STRING(lhs), LuaValue::STRING(rhs)) => lhs == rhs,
            (LuaValue::USERDATA(lhs), LuaValue::USERDATA(rhs)) => lhs == rhs,
            (LuaValue::FUNCTION(lhs), LuaValue::FUNCTION(rhs)) => lhs == rhs,
            (LuaValue::THREAD(lhs), LuaValue::THREAD(rhs)) => lhs == rhs,
            (LuaValue::TABLE(lhs), LuaValue::TABLE(rhs)) => lhs == rhs,
            _ => false
        }
    }
}

impl PartialOrd for LuaValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (LuaValue::NUMBER(lhs), LuaValue::NUMBER(rhs)) => lhs.partial_cmp(rhs),
            (LuaValue::STRING(lhs), LuaValue::STRING(rhs)) => lhs.partial_cmp(rhs),
            _ => None
        }
    }
}

struct LuaValueFullEq {
    inner: LuaValue
}

impl LuaValue {
    fn try_full_eq(self) -> Result<LuaValueFullEq, ()> {
        if self == self {
            return Ok(LuaValueFullEq { inner: self });
        } else {
            Err(())
        }
    }
}

impl Eq for LuaValueFullEq {}

impl PartialEq for LuaValueFullEq {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl Hash for LuaValueFullEq {
    fn hash<H: Hasher>(&self, state: &mut H) {  // TODO: Possibly implement specific hashing algorithm
        match &self.inner {
            LuaValue::NIL => unreachable!(),    // NIL is an invalid key, and thus should never be hashed
            LuaValue::BOOLEAN(b) => state.write_u8(if *b { 1 } else { 0 }),
            LuaValue::NUMBER(n) => {
                match n {
                    LuaNumber::INT(int) => state.write_i64(*int),
                    LuaNumber::FLOAT(float) => state.write(&float.to_le_bytes()),
                }
            }
            LuaValue::STRING(s) => state.write(s.as_bytes()),
            LuaValue::USERDATA(u) => state.write_usize(u.inner.as_ref() as *const UserDataImpl as *const () as usize),
            LuaValue::FUNCTION(f) => match f {
                LuaFunction::CLOSURE(c) => state.write_usize(c.as_ref().as_ptr() as *const () as usize),
                LuaFunction::RUST(f) => state.write_usize(f as *const fn(&[LuaValue]) -> Result<Varargs, ExecuteError> as *const () as usize)
            },
            LuaValue::THREAD(t) => unimplemented!(),
            LuaValue::TABLE(t) => state.write_usize(t.inner.as_ref() as *const (RefCell<TableImpl>, Option<LuaTable>) as *const () as usize),
        }
    }
}

impl Display for LuaValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            LuaValue::NIL => write!(f, "NIL"),
            LuaValue::BOOLEAN(b) => write!(f, "{}", b),
            LuaValue::NUMBER(n) => write!(f, "{}", n),
            LuaValue::STRING(s) => write!(f, "{}", s),
            LuaValue::USERDATA(u) => write!(f, "{}", u),
            LuaValue::FUNCTION(function) => write!(f, "{}", function),
            LuaValue::THREAD(thread) => write!(f, "{}", thread),
            LuaValue::TABLE(table) => write!(f, "{}", table),
        }
    }
}

#[derive(PartialEq, Copy, Clone)]
pub enum LuaNumber {
    INT(LUA_INT),
    FLOAT(LUA_FLOAT),
}

impl LuaNumber {
    pub fn as_int(&self) -> Result<LUA_INT, MathError> {
        match self {
            LuaNumber::INT(int) => Ok(*int),
            LuaNumber::FLOAT(float) => {
                if float.is_finite() {
                    Ok(*float as LUA_INT)
                } else {
                    Err(MathError::CannotConvertNonFiniteFloatToInt)
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
        match (lhs.as_int(), rhs.as_int()) {
            (Ok(l_i), Ok(r_i)) => {
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
        match num.try_into() {
            Ok(n) => LuaNumber::INT(n),
            Err(_) => LuaNumber::FLOAT(num as f64)
        }
    }
}

macro_rules! delegate_math_to_primitive {
    ($trait:ident, $func:ident, $int_func:ident, $float_op:tt) => {
        impl $trait for LuaNumber {
            type Output = Result<LuaNumber, MathError>;

            fn $func(self, rhs: Self) -> Self::Output {
                Ok(LuaNumber::binary_op(self, rhs, |lhs, rhs| {lhs.$int_func(rhs)}, |lhs, rhs| {lhs $float_op rhs}))
            }
        }
    };
    ($trait:ident, $func:ident, intonly: $float_op:tt) => {
        impl $trait for LuaNumber {
            type Output = Result<LuaNumber, MathError>;

            fn $func(self, rhs: Self) -> Self::Output {
                Ok(LuaNumber::INT(self.as_int()? $float_op rhs.as_int()?))
            }
        }
    };
    ($trait:ident, $func:ident, unary: $op:tt) => {
        impl $trait for LuaNumber {
            type Output = Result<LuaNumber, MathError>;

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
    pub fn pow(self, rhs: Self) -> Result<LuaNumber, MathError> {
        Ok(LuaNumber::binary_op(
            self, rhs,
            |lhs, rhs| {
                rhs.try_into().ok().and_then(|rhs| { lhs.checked_pow(rhs) })
            },
            |lhs, rhs| { lhs.powf(rhs) }))
    }

    pub fn idiv(self, rhs: Self) -> Result<LuaNumber, MathError> {
        Ok((self / rhs)?.floor())
    }

    pub fn bnot(self) -> Result<LuaNumber, MathError> {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            LuaNumber::INT(integer) => write!(f, "{}", integer),
            LuaNumber::FLOAT(float) => write!(f, "{}", float),
        }
    }
}

#[derive(Clone, Debug)]
pub enum LuaString {
    UNICODE(Rc<str>),
    BINARY(Rc<[u8]>),
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

    pub fn append(self, other: LuaString) -> LuaString {
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

    pub fn try_utf8(&self) -> Result<&str, TypeCastError> {
        match self {
            LuaString::UNICODE(unicode) => Ok(unicode),
            LuaString::BINARY(bytes) => {
                debug_assert!(std::str::from_utf8(bytes).is_err());
                Err(TypeCastError { expected: "string:utf8", found: "string:binary" })
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

impl Display for LuaString {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
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

impl PartialOrd for LuaString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_bytes().partial_cmp(&other.as_bytes())
    }
}

struct TableImpl {
    map: HashMap<LuaValueFullEq, LuaValue>,
    array: Vec<LuaValue>,
}

impl Display for TableImpl {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        unimplemented!()
    }
}

#[derive(Debug)]
pub struct KeyError(&'static str);

#[derive(Clone)]
pub struct LuaTable {
    inner: Rc<(RefCell<TableImpl>, Option<LuaTable>)>   // Where inner.0 = TableImpl, and inner.1 = table's metatable
}

impl LuaTable {
    pub fn empty() -> LuaTable {
        LuaTable {
            inner: Rc::new((RefCell::new(TableImpl {
                map: HashMap::new(),
                array: vec![],
            }), None))
        }
    }

    pub fn with_capacity(array_capacity: usize, hash_capacity: usize) -> LuaTable {
        LuaTable {
            inner: Rc::new((RefCell::new(TableImpl {
                map: HashMap::with_capacity(hash_capacity),
                array: Vec::with_capacity(array_capacity),
            }), None))
        }
    }

    // Note to self: _DO NOT ENTER KEY/VALUE REFCELLS IN THIS METHOD_
    // Note: Does not invoke metamethod, TODO: Implement in vm
    pub fn get(&self, key: &LuaValue) -> Result<LuaValue, KeyError> {
        if key == &LuaValue::NIL { return Ok(LuaValue::NIL); }

        let (inner, _) = &*self.inner;
        let table = inner.borrow_mut();
        match key {
            LuaValue::NUMBER(num) if num.as_int().map(|i| { i >= 0 && ((i as usize) < table.array.len()) }).unwrap_or(false) => {
                let index = num.as_int().unwrap() as usize;
                if let Some(val) = table.array.get(index) {
                    if val != &LuaValue::NIL {
                        return Ok(val.clone());
                    }
                }
            }
            _ => {
                let key = key.clone().try_full_eq().map_err(|_| { KeyError("table key is NaN") })?;
                if let Some(val) = table.map.get(&key) {
                    if val != &LuaValue::NIL {
                        return Ok(val.clone());
                    }
                }
            }
        };
        Ok(LuaValue::NIL)
    }

    // Note to self: _DO NOT ENTER KEY/VALUE REFCELLS IN THIS METHOD_
    // Note: Does not invoke metamethod, TODO: Implement in vm
    // TODO: Ensure tables are a DAG
    pub fn set(&self, key: LuaValue, value: LuaValue) -> Result<(), KeyError> {
        if key == LuaValue::NIL {
            return Err(KeyError("table key is NIL"));
        }
        let key_eq = key.clone().try_full_eq().map_err(|_| KeyError("table key is NaN"))?;
        let (inner, _) = &*self.inner;
        let table = &mut *inner.borrow_mut();

        match key {
            LuaValue::NUMBER(num) if num.as_int().map(|i| { i >= 0 && ((i as usize) < table.array.len()) }).unwrap_or(false) => {
                let index = num.as_int().unwrap() as usize;
                if value == LuaValue::NIL && index == table.array.len() - 1 {
                    table.array.truncate(table.array.len() - 1);
                    while let Some(LuaValue::NIL) = table.array.last() {
                        table.array.truncate(table.array.len() - 1);
                    }
                } else {
                    mem::replace(table.array.get_mut(index).unwrap(), value);
                }
            }
            LuaValue::NUMBER(num) if num.as_int().map(|i| { i >= 0 && ((i as usize) == table.array.len()) }).unwrap_or(false) => {
                if value != LuaValue::NIL {
                    if let Some(_) = table.map.get(&key_eq) {
                        table.map.remove(&key_eq);
                    }
                    table.array.push(value)
                } else if let Some(dest) = table.map.get_mut(&key_eq) {
                    mem::replace(
                        dest,
                        value,
                    );
                }
            }
            _ => {
                if value == LuaValue::NIL {
                    table.map.remove(&key_eq);
                } else {
                    table.map.insert(key_eq, value);
                }
            }
        };
        Ok(())
    }

    pub fn metatable(&self) -> &Option<LuaTable> {
        &self.inner.1
    }

    pub fn len(&self) -> usize {
        let (inner, metatable) = &*self.inner;
        let tab = inner.borrow();
        tab.array.len() + tab.map.len()
    }
}

impl Display for LuaTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.inner.0.borrow())
    }
}

impl PartialEq for LuaTable {
    fn eq(&self, other: &Self) -> bool {
        &*self.inner as *const (RefCell<TableImpl>, Option<LuaTable>) == &*other.inner as *const (RefCell<TableImpl>, Option<LuaTable>)
    }
}

struct UserDataImpl {
    pub metatable: Option<LuaTable>,
    pub value: dyn Any,
}

#[derive(Clone)]
pub struct UserData {
    inner: Rc<UserDataImpl>,
}

impl UserData {
    pub fn metatable(&self) -> &Option<LuaTable> {
        &self.inner.metatable
    }
}

impl Display for UserData {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "USERDATA@{}", &self.inner.value as *const dyn Any as *const () as usize)
    }
}

impl PartialEq for UserData {
    fn eq(&self, other: &Self) -> bool {
        &*self.inner as *const UserDataImpl == &*other.inner as *const UserDataImpl
    }
}

pub struct Prototype {
    // TODO: Extract debug info to it's own type
    pub source_string: Option<LuaString>,
    pub first_line_defined: HOST_INT,
    pub last_line_defined: HOST_INT,
    pub param_count: u8,
    pub is_vararg: u8,
    pub max_stack_size: u8,
    pub code: Vec<LUA_INSTRUCTION>,
    pub constants: Vec<LuaValue>,
    pub upvalues: Vec<UpvalueDesc>,
    pub functions: Vec<Rc<Prototype>>,
    pub lineinfo: Vec<HOST_INT>,
    pub locvars: Vec<LocVar>,
    pub upvaluenames: Vec<Option<LuaString>>,
}

impl Prototype {
    pub fn from_parts(
        source_string: Option<LuaString>,
        first_line_defined: HOST_INT,
        last_line_defined: HOST_INT,
        param_count: u8,
        is_vararg: u8,
        max_stack_size: u8,
        code: Vec<LUA_INSTRUCTION>,
        constants: Vec<LuaValue>,
        upvalues: Vec<UpvalueDesc>,
        functions: Vec<Rc<Prototype>>,
        lineinfo: Vec<HOST_INT>,
        locvars: Vec<LocVar>,
        upvaluenames: Vec<Option<LuaString>>,
    ) -> Self {
        Prototype {
            source_string,
            first_line_defined,
            last_line_defined,
            param_count,
            is_vararg,
            max_stack_size,
            code,
            constants,
            upvalues,
            functions,
            lineinfo,
            locvars,
            upvaluenames,
        }
    }
}

impl Display for Prototype {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        for (index, instruction) in self.code.iter().enumerate() {
            let opcode: u8 = (instruction & 0b111111) as u8;
            let a = (instruction >> 6) as usize & 0b1111_1111;
            let b = (instruction >> 23) as usize & 0b1_1111_1111;
            let c = (instruction >> 14) as usize & 0b1_1111_1111;
            let bx = (instruction >> 14) as usize & 0b11_1111_1111_1111_1111;
            let sbx = ((instruction >> 14) as usize & 0b11_1111_1111_1111_1111) as isize - (18isize.pow(2) / 2);
            write!(f, "{}\t{}\ta:{} b:{} c:{} bx:{} sbx:{}\n", index, constants::opcodes::name(opcode), a, b, c, bx, sbx)?;
        }
        write!(f, "CONSTANTS: {}\n", self.constants.len())?;
        for (index, constant) in self.constants.iter().enumerate() {
            write!(f, "{}\t{}\n", index, constant)?;
        }

        write!(f, "UPVALUES: {}\n", self.upvalues.len())?;
        for (index, upvalue) in self.upvalues.iter().enumerate() {
            let unnamed = LuaString::UNICODE(Rc::from("Unnamed upvalue"));
            let name = &match match self.upvaluenames.get(index) {
                None => &None,
                Some(opt) => opt,
            } {
                Some(name) => name,
                None => &unnamed,
            };
            write!(f, "{}\t{}\t{}\n", name, index, upvalue)?;
        }

        write!(f, "FUNCTIONS: {}\n", self.functions.len())?;
        for (index, function) in self.functions.iter().enumerate() {
            write!(f, "Function-{}\n{}", index, function)?;
        }
        Ok(())
    }
}

pub struct LuaClosure {
    // TODO: Ensure upvalues are a DAG!
    pub proto: Rc<Prototype>,
    pub upvalues: Vec<Upvalue>,
    pub parent: Option<Rc<RefCell<LuaClosure>>>, // what?
}

#[derive(Clone)]
pub enum LuaFunction {
    CLOSURE(Rc<RefCell<LuaClosure>>),
    RUST(fn(&[LuaValue]) -> Result<Varargs, ExecuteError>)
}

impl Eq for LuaFunction {}

impl PartialEq for LuaFunction {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LuaFunction::CLOSURE(lhs), LuaFunction::CLOSURE(rhs)) => RefCell::as_ptr(lhs) as *const LuaClosure == RefCell::as_ptr(rhs) as *const LuaClosure,
            (LuaFunction::RUST(lhs), LuaFunction::RUST(rhs)) => lhs as *const fn(&[LuaValue]) -> Result<Varargs, ExecuteError> == rhs as *const fn(&[LuaValue]) -> Result<Varargs, ExecuteError>,
            _ => false
        }
    }
}

impl Display for LuaFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            LuaFunction::CLOSURE(closure) => write!(f, "{}", closure.borrow().proto),
            LuaFunction::RUST(function) => write!(f, "Rust:{:X}", function as *const fn(&[LuaValue]) -> Result<Varargs, ExecuteError> as *const () as usize),
        }
    }
}

#[derive(Clone)]
pub struct LuaThread {
    // TODO: Implement "threads"/coroutines
}

impl Display for LuaThread {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        unimplemented!()
    }
}

impl PartialEq for LuaThread {
    fn eq(&self, other: &Self) -> bool {
        unimplemented!()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
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
}

impl Display for UpvalueDesc {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "instack: {}\tidx:{}", self.instack, self.idx)
    }
}

#[derive(Clone)]
pub enum UpvalueImpl {
    Open { frame: usize, register: usize },
    Closed(LuaValue),
}

#[derive(Clone)]
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

pub struct LocVar {
    name: Option<LuaString>,
    startpc: HOST_INT,
    endpc: HOST_INT,
}

impl LocVar {
    pub fn new(name: Option<LuaString>, startpc: HOST_INT, endpc: HOST_INT) -> Self {
        LocVar { name, startpc, endpc }
    }
}