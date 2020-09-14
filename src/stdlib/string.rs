use crate::vm;
use crate::vm::ExecutionState;
use crate::error::{TracedError, ArgumentError, Traceable};
use crate::stdlib::string::pattern::compile_pattern;
use nom::{FindSubstring, AsBytes, IResult};
use crate::util::Union3;
use regex::bytes::Captures;

use crate::types::value::LuaValue;
use crate::types::value::string::LuaString;
use crate::types::varargs::Varargs;
use crate::types::value::function::{LuaFunction, LuaClosure};
use crate::types::value::table::LuaTable;
use crate::types::parameters::LuaParameters;
use crate::types::{LuaType, CoerceFrom};
use crate::constants::types::LUA_INT;
use nom::lib::std::fmt::{Debug, Formatter};
use std::fmt;
use nom::multi::{many0, many1};
use nom::branch::alt;
use nom::combinator::{map, opt};
use nom::character::complete::none_of;
use nom::bytes::complete::{tag, is_a};
use nom::sequence::tuple;

fn relative_index_to_absolute(index: i64, string: &LuaString) -> usize {
    if index >= 0 {
        index as usize
    } else {
        string.len().saturating_sub((-index) as usize)
    }
}

pub fn byte(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let string = params.try_coerce::<LuaString>(0)?;
        let start = if let Some(LuaValue::NUMBER(n)) = params.get(1) {
            n.as_int()? - 1
        } else {
            0
        };
        let end = if let Some(LuaValue::NUMBER(n)) = params.get(2) {
            n.as_int()? - 1
        } else {
            start
        };
        // Translate negative indices to actual indices
        let start: usize = relative_index_to_absolute(start, &string);
        let end: usize = relative_index_to_absolute(end, &string);

        if end >= start &&
            start < string.len() &&
            end < string.len() {
            let return_vec: Vec<LuaValue> = string.as_bytes()[start..=end].iter().map(|b| LuaValue::from(*b as usize)).collect();
            Varargs::from(return_vec)
        } else {
            Varargs::empty()
        }
    };
    result.trace(byte)
}

pub fn char(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let mut vec = Vec::new();
    for (index, param) in params.iter().enumerate() {
        match param {
            LuaValue::NUMBER(num) if (0..255_i64).contains(&num.as_int().unwrap_or(-1)) => { // Check that "num" is an integer between 0 and 255
                vec.push(num.as_int().unwrap() as u8);
            }
            _ => return Err(TracedError::from_rust(ArgumentError::InvalidArgument { expected: "byte (0-254)".to_string(), found: params.get_value_or_nil(index).type_name(), index }, char))
        }
    }

    Ok(Varargs::from(LuaString::from(vec.into_boxed_slice())))
}

pub fn dump(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> { // TODO: Implement strip option
    let result: Result<Varargs, ArgumentError> = try {
        let closure = params.try_coerce::<LuaClosure>(0)?;
        let mut bytes = Vec::new();
        crate::bytecode::dumper::dump_chunk(&closure.borrow().proto, &mut bytes);
        Varargs::from(LuaString::from(bytes.into_boxed_slice()))
    };
    result.trace(dump)
}

pub fn find(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let string = params.try_coerce::<LuaString>(0)?;
        let pattern = params.try_coerce::<LuaString>(1)?;
        let rel_index = params.get(2)
            .map(|v| if let LuaValue::NUMBER(n) = v { n.as_int().unwrap_or(1) - 1 } else { 0 })
            .unwrap_or(0);
        let start_index = {
            let index = relative_index_to_absolute(
                rel_index,
                &string,
            );
            if index < string.len() {
                index
            } else {
                string.len().saturating_sub(1)  // prevent overflow
            }
        };

        let plain_match = params.get(3).map(|v| if let LuaValue::BOOLEAN(b) = v { *b } else { false }).unwrap_or(false);
        if !plain_match {
            let regex = compile_pattern(pattern.as_bytes())?;
            match regex.captures(&string.as_bytes()[start_index..]) {
                Some(captures) => {
                    let mut return_values = Vec::new();
                    let m = captures.get(0).expect("Captures must always match whole group");
                    return_values.push(LuaValue::from(m.start() + 1));
                    return_values.push(LuaValue::from(m.end() + 1));
                    for capture in captures.iter() {
                        if let Some(m) = capture {
                            return_values.push(LuaValue::from(m.as_bytes()))
                        }
                    }
                    Varargs::from(return_values)
                }
                None => Varargs::nil()
            }
        } else {
            match string.as_bytes().find_substring(pattern.as_bytes()) {
                Some(m) => Varargs::from((m + 1, m + pattern.len())),
                None => Varargs::nil()
            }
        }
    };
    result.trace(find)
}

#[derive(Debug)]
struct FormatFlags {
    left_justify: bool,
    force_sign: bool,
    pad_sign: bool,
    hash: bool,
}

enum FormatElement {
    RawString(Vec<u8>),
    Format {
        flags: FormatFlags,
        width: Option<(usize, bool)>,
        precision: Option<usize>,
        specifier: u8,
    },
}

impl Debug for FormatElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            FormatElement::RawString(vec) => {
                f.debug_tuple("FormatElement").field(&String::from_utf8_lossy(&vec[..])).finish()
            }
            FormatElement::Format { flags, width, precision, specifier } => {
                f.debug_struct("FormatElement")
                    .field("flags", flags)
                    .field("width", width)
                    .field("precision", precision)
                    .field("specifier", &(*specifier as char))
                    .finish()
            }
        }
    }
}

type StringFormat = Vec<FormatElement>;

fn parse_format(i: &[u8]) -> IResult<&[u8], StringFormat> {
    many0(
        alt((
            map(
                many1(none_of("%")),
                |v| FormatElement::RawString(v.iter().map(|c| *c as u8).collect()),
            ),
            map(
                tag("%%"),
                |_| FormatElement::RawString(Vec::from(&b"%"[..])),
            ),
            map(
                tuple((
                    tag("%"),
                    map(
                        opt(is_a("-+ #")),
                        |v: Option<&[u8]>| {
                            let v = v.unwrap_or(b"");
                            FormatFlags {
                                left_justify: v.contains(&b'-'),
                                force_sign: v.contains(&b'+'),
                                pad_sign: v.contains(&b' '),
                                hash: v.contains(&b'#'),
                            }
                        },
                    ),
                    map(
                        opt(is_a("0123456789")),
                        |num: Option<&[u8]>| {
                            let num = num.unwrap_or(b"");
                            if num.len() > 0 {
                                Some((std::str::from_utf8(num).unwrap().parse::<usize>().unwrap(), num[0] == 0))
                            } else {
                                None
                            }
                        },
                    ),
                    opt(tuple((
                        tag("."),
                        map(
                            is_a("0123456789"),
                            |num: &[u8]| if num.len() > 0 {
                                Some(std::str::from_utf8(num).unwrap().parse::<usize>().unwrap())
                            } else {
                                Some(0)
                            },
                        )
                    ))),
                    alt((
                        tag("d"), tag("i"), tag("u"), tag("o"), tag("x"), tag("X"), tag("f"), tag("F"),
                        tag("e"), tag("E"), tag("g"), tag("G"), tag("a"), tag("A"), tag("c"), tag("s"),
                        tag("p"), tag("n"), tag("q")
                    ))
                )),
                |tuple| FormatElement::Format {
                    flags: tuple.1,
                    width: tuple.2,
                    precision: tuple.3.and_then(|t| t.1),
                    specifier: tuple.4[0],
                },
            )
        ))
    )(i)
}

// TODO: This function is a mess and needs to be fixed
pub fn format(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {   // TODO: Extract formatting to the utility module
    let result: Result<Varargs, ArgumentError> = try {
        let format_string = params.try_coerce::<LuaString>(0)?;
        match parse_format(format_string.as_bytes()) {
            Ok((_, string_format)) => {
                let mut index = 1;
                let mut results = Vec::new();
                for element in string_format {
                    match element {
                        FormatElement::RawString(string) => results.extend_from_slice(&string[..]),
                        FormatElement::Format { flags, width: width_options, precision: precision_option, specifier } => {
                            match specifier as char {
                                'd' | 'i' => {
                                    let value = params.try_coerce::<LUA_INT>(index)?;
                                    index += 1;

                                    // Convert value to string without sign
                                    let mut string = value.abs().to_string();

                                    match (width_options, precision_option) {
                                        (None, Some(precision)) => {
                                            // Pad digits to [precision] with zeros
                                            string = format!("{:0width$}", string, width = precision);
                                            if value < 0 {
                                                string = format!("-{}", string);
                                            } else if flags.force_sign {
                                                string = format!("+{}", string);
                                            }
                                        }
                                        (Some((width, pad_zeros)), None) => {
                                            if flags.left_justify {     // Pads with space regardless of pad_zero flag
                                                // Add sign
                                                if value < 0 {
                                                    string = format!("-{}", string);
                                                } else if flags.force_sign {
                                                    string = format!("+{}", string);
                                                }
                                                string = format!("{:<width$}", string, width = width);
                                            } else {
                                                if pad_zeros {
                                                    string = format!("{:0width$}", string, width = width.saturating_sub(0));
                                                    // Add sign
                                                    if value < 0 {
                                                        string = format!("-{}", string);
                                                    } else if flags.force_sign {
                                                        string = format!("+{}", string);
                                                    } else {
                                                        string = format!(" {}", string);
                                                    }
                                                } else {
                                                    // Add sign
                                                    if value < 0 {
                                                        string = format!("-{}", string);
                                                    } else if flags.force_sign {
                                                        string = format!("+{}", string);
                                                    }
                                                    string = format!("{:width$}", string, width = width.saturating_sub(0));
                                                }
                                            }
                                        }
                                        (Some((width, _)), Some(precision)) => {
                                            string = format!("{:0width$}", string, width = precision);
                                            if value < 0 {
                                                string = format!("-{}", string);
                                            } else if flags.force_sign {
                                                string = format!("+{}", string);
                                            }

                                            if flags.left_justify {     // Pads with space regardless of pad_zero flag
                                                string = format!("{:<width$}", string, width = width);
                                            } else {
                                                string = format!("{:width$}", string, width = width.saturating_sub(0));
                                            }
                                        }
                                        (None, None) => {
                                            if value < 0 {
                                                string = format!("-{}", string);
                                            } else if flags.force_sign {
                                                string = format!("+{}", string);
                                            }
                                        }
                                    }
                                    results.extend_from_slice(string.as_bytes())
                                }
                                // 'u' => {}
                                // 'o' => {}
                                // 'x' | 'X' => {}
                                // 'f' | 'F' => {}
                                // 'e' | 'E' => {}
                                // 'g' | 'G' => {}
                                // 'a' | 'A' => {}
                                // 'c' => {}
                                // 's' => {}
                                // 'q' => {}
                                _ => unreachable!("Unexpected specifier: {}", specifier)
                            }
                        }
                    };
                }

                Varargs::from(LuaString::from(results.into_boxed_slice()))
            }
            Err(err) => {
                return Err(TracedError::from_rust(ArgumentError::InvalidPatternOrFormat { message: format!("{}", err) }, format));
            }
        }
    };
    result.trace(format)
}

pub fn gmatch(_execstate: &mut ExecutionState, _params: &[LuaValue]) -> Result<Varargs, TracedError> {
    unimplemented!()
}

#[derive(Debug)]
enum ReplacementElement {
    RawString(u8),
    Capture(u8),
}

fn parse_repl(input: &[u8]) -> Result<Vec<ReplacementElement>, ArgumentError> {
    let mut vec = Vec::new();
    let mut iter = input.iter();
    while let Some(byte) = iter.next() {
        match byte {
            b'%' => {
                if let Some(escaped) = iter.next() {
                    match escaped {
                        b'%' => vec.push(ReplacementElement::RawString(b'%')),
                        b'0'..=b'9' => vec.push(ReplacementElement::Capture(escaped - b'0')),
                        _ => return Err(ArgumentError::InvalidPatternOrFormat { message: format!("Unexpected escape character: {}", *byte as char) })
                    }
                } else {
                    return Err(ArgumentError::InvalidPatternOrFormat { message: "Replacement string ends with escape!".to_string() });
                }
            }
            _ => vec.push(ReplacementElement::RawString(*byte))
        }
    }
    Ok(vec)
}

pub fn gsub(execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let string = params.try_coerce::<LuaString>(0)?;
        let pattern = params.try_coerce::<LuaString>(1)?;

        let repl = match params.get(2) {
            Some(LuaValue::STRING(s)) => {
                match parse_repl(s.as_bytes()) {
                    Ok(replacement) => {
                        Union3::One(replacement)
                    }
                    Err(err) => {
                        return Err(TracedError::from_rust(err, gsub));
                    }
                }
            }
            Some(LuaValue::TABLE(t)) => Union3::Two(t),
            Some(LuaValue::FUNCTION(f)) => Union3::Three(f),
            _ => return Err(TracedError::from_rust(ArgumentError::InvalidArgument { expected: "string, table or function".to_string(), found: params.get_value_or_nil(2).type_name(), index: 2 }, gsub))
        };
        // TODO Check 'as usize' conversion for negative numbers elsewhere; it's a mem::transmute!
        let n = params.try_coerce::<LUA_INT>(3).ok().map(|i| i.max(0) as usize);

        if n.contains(&0) {
            return Ok(Varargs::from((string.clone(), 0usize)));
        }

        let mut replacement_func_call_error = None;


        let regex = compile_pattern(pattern.as_bytes())?;
        let mut substitution_count = 0usize;
        let result = match repl {
            Union3::One(repl) => {
                regex.replacen(
                    string.as_bytes(),
                    n.unwrap_or(std::usize::MAX),
                    |captures: &Captures| {
                        substitution_count += 1;
                        let mut replacement: Vec<u8> = Vec::new();
                        for element in &repl {
                            match element {
                                ReplacementElement::RawString(s) => replacement.push(*s),
                                ReplacementElement::Capture(c) => {
                                    if let Some(m) = captures.get(*c as usize) {
                                        replacement.extend_from_slice(m.as_bytes())
                                    }
                                }
                            }
                        }
                        replacement
                    },
                )
            }
            Union3::Two(table) => {
                regex.replacen(
                    string.as_bytes(),
                    n.unwrap_or(std::usize::MAX),
                    |captures: &Captures| {
                        substitution_count += 1;
                        let key = LuaValue::from(captures.get(0).expect("gsub replacement without 0-capture!").as_bytes());
                        if let Some(s) = table.raw_get(&key).ok().as_ref().map(LuaString::coerce_from).and_then(Result::ok) {   // TODO: Check if metatable __index applies here
                            s
                        } else {
                            LuaString::from("")
                        }
                    },
                )
            }
            Union3::Three(function) => {
                regex.replacen(
                    string.as_bytes(),
                    n.unwrap_or(std::usize::MAX),
                    |captures: &Captures| {
                        substitution_count += 1;
                        if replacement_func_call_error.is_none() {
                            let arguments: Vec<LuaValue> = captures.iter().map(|c| c.map(|m| LuaValue::from(LuaString::from(m.as_bytes()))).unwrap_or(LuaValue::NIL)).collect();
                            let args = if arguments.len() > 1 {
                                &arguments[1..]
                            } else {
                                &arguments[..]
                            };
                            match vm::helper::do_call_from_rust(gsub, LuaValue::from((*function).clone()), execstate, args) {
                                Ok(result) => {
                                    if let Some(s) = result.opt(0).map(LuaString::coerce_from).and_then(Result::ok) {
                                        s
                                    } else {
                                        LuaString::from(captures.get(0).expect("gsub replacement without 0-capture!").as_bytes())
                                    }
                                }
                                Err(err) => {
                                    replacement_func_call_error.replace(err);
                                    LuaString::from("")
                                }
                            }
                        } else {
                            LuaString::from("")
                        }
                    },
                )
            }
        };

        Varargs::from((result.as_bytes(), substitution_count))
    };
    result.trace(gsub)
}

pub fn len(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let string = params.try_coerce::<LuaString>(0)?;
        Varargs::from(string.len())
    };
    result.trace(len)
}

pub fn lower(_execstate: &mut ExecutionState, _params: &[LuaValue]) -> Result<Varargs, TracedError> {
    unimplemented!()
}

pub fn string_match(_execstate: &mut ExecutionState, _params: &[LuaValue]) -> Result<Varargs, TracedError> {
    unimplemented!()
}

pub fn pack(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let _format_string = params.try_coerce::<LuaString>(0)?;
        debug_assert!(params.len() > 0);    // We can assume this as the above fails with length 0
        let _arguments = &params[1..];

        unimplemented!()
    };
    result.trace(format)
}

pub fn packsize(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    use std::mem;
    use crate::constants::types::*;

    let result: Result<Varargs, ArgumentError> = try {
        let format_string = params.try_coerce::<LuaString>(0)?;
        let mut size = 0usize;  // TODO: perhaps check overflows?
        for byte in format_string.as_bytes() {
            size += match byte {
                b'b' | b'B' => mem::size_of::<HOST_BYTE>(),
                b'j' | b'J' => mem::size_of::<LUA_INT>(),
                _ => Err(ArgumentError::InvalidPatternOrFormat { message: format!("Unknown pack format option: {}", *byte as char) })?
            };
        }
        Varargs::from(size)
    };
    result.trace(format)
}

pub fn rep(_execstate: &mut ExecutionState, params: &[LuaValue]) -> Result<Varargs, TracedError> {
    let result: Result<Varargs, ArgumentError> = try {
        let string = params.try_coerce::<LuaString>(0)?;
        let repetitions = params.try_coerce::<LUA_INT>(1)?;
        let separator = params.try_coerce::<LuaString>(2).ok();

        if repetitions > 0 {
            let size = string.len()
                .checked_mul(repetitions as usize)  // Multiply string length by repetitions to get length of the new string before the separator
                .and_then(|s| // Add cumulative length of the separators
                    separator.as_ref().map(LuaString::len).unwrap_or(0usize)
                        .checked_mul(repetitions as usize)
                        .and_then(|sep_len| sep_len.checked_add(s))
                )
                .ok_or(ArgumentError::ConcatenationTooLarge)?;  // If we overflowed anywhere in the above calculation, raise the ConcatenationTooLarge error
            // TODO: Sensibility check on the above size, as it currently permits strings into the exabyte range

            let buffer = if separator.is_none() || separator.as_ref().map(LuaString::len).contains(&0) {
                string.as_bytes().repeat(repetitions as usize)
            } else {
                let separator = separator.unwrap(); // We already checked is_none above.
                let mut buffer = Vec::with_capacity(size);
                for i in 0..repetitions {
                    if i != 0 {
                        buffer.extend_from_slice(separator.as_bytes())
                    }
                    buffer.extend_from_slice(string.as_bytes())
                }
                buffer
            };

            Varargs::from(LuaString::from(buffer.into_boxed_slice()))
        } else {
            Varargs::from(LuaString::from(""))
        }
    };
    result.trace(rep)
}

pub fn reverse(_execstate: &mut ExecutionState, _params: &[LuaValue]) -> Result<Varargs, TracedError> {
    unimplemented!()
}

pub fn sub(_execstate: &mut ExecutionState, _params: &[LuaValue]) -> Result<Varargs, TracedError> {
    unimplemented!()
}

pub fn unpack(_execstate: &mut ExecutionState, _params: &[LuaValue]) -> Result<Varargs, TracedError> {
    unimplemented!()
}

pub fn upper(_execstate: &mut ExecutionState, _params: &[LuaValue]) -> Result<Varargs, TracedError> {
    unimplemented!()
}

pub fn insert_string_lib(execstate: &mut ExecutionState) {
    let table = LuaTable::empty();

    set_table!(table, byte);
    set_table!(table, char);
    set_table!(table, dump);
    set_table!(table, find);
    set_table!(table, format);
    set_table!(table, gmatch);
    set_table!(table, gsub);
    set_table!(table, len);
    set_table!(table, lower);
    set_table!(table, "match", string_match);
    set_table!(table, pack);
    set_table!(table, packsize);
    set_table!(table, rep);
    set_table!(table, reverse);
    set_table!(table, sub);
    set_table!(table, unpack);
    set_table!(table, upper);

    let metatable = LuaTable::empty();
    metatable.raw_set("__index", table.clone()).expect("Raw set with string key should not error!");
    execstate.metatables.string.replace(metatable);
    execstate.global_env.raw_set("string", table.clone()).expect("Raw set with string key should not error!");
    execstate.modules.insert("string", table.clone());
}

pub mod pattern {
    use crate::error::ArgumentError;
    use regex::bytes::Regex;
    use nom::branch::alt;
    use nom::combinator::map;
    use nom::bytes::complete::{tag, take};
    use nom::sequence::{preceded, tuple};
    use nom::character::complete::none_of;
    use nom::IResult;
    use nom::multi::many1;

    trait PatternComponent {
        fn compile(&self, buf: &mut Vec<u8>) -> Result<(), ArgumentError>;

        fn escape(byte: u8, buf: &mut Vec<u8>) {
            match byte as char {
                '.' | '^' | '$' | '*' | '+' | '?' | '(' | ')' | '[' | '{' | '\\' | '|' => buf.extend_from_slice(&['\\' as u8, byte]),
                _ => buf.push(byte)
            }
        }

        fn escape_set(byte: u8, buf: &mut Vec<u8>) {
            match byte as char {
                '^' | '-' | ']' | '\\' => buf.extend_from_slice(&['\\' as u8, byte]),
                _ => buf.push(byte)
            }
        }
    }

    #[derive(Debug)]
    enum CharacterClass {
        Character(u8),
        All,
        Letter,
        ControlCharacter,
        Digit,
        Printable,
        Lowercase,
        Punctuation,
        Space,
        Uppercase,
        Alphanumeric,
        Hexadecimal,
        NotLetter,
        NotControlCharacter,
        NotDigit,
        NotPrintable,
        NotLowercase,
        NotPunctuation,
        NotSpace,
        NotUppercase,
        NotAlphanumeric,
        NotHexadecimal,
    }

    impl PatternComponent for CharacterClass {
        fn compile(&self, buf: &mut Vec<u8>) -> Result<(), ArgumentError> {
            match self {
                CharacterClass::Character(c) => CharacterClass::escape(*c, buf),
                CharacterClass::All => buf.push('.' as u8),
                CharacterClass::Letter => buf.extend_from_slice(b"\x41-\x5a\x61-\x7a"),
                CharacterClass::ControlCharacter => buf.extend_from_slice(b"\x00-\x1f\x7f-\x7f"),
                CharacterClass::Digit => buf.extend_from_slice(b"\x30-\x39"),
                CharacterClass::Printable => buf.extend_from_slice(b"\x21-\x7E"),
                CharacterClass::Lowercase => buf.extend_from_slice(b"\x61-\x7A"),
                CharacterClass::Punctuation => buf.extend_from_slice(b"\x21-\x2F\x3A-\x40\x5B-\x60\x7B-\x7E"),
                CharacterClass::Space => buf.extend_from_slice(b"\x09-\x0D\x20-\x20"),
                CharacterClass::Uppercase => buf.extend_from_slice(b"\x41-\x5A"),
                CharacterClass::Alphanumeric => buf.extend_from_slice(b"\x30-\x39\x41-\x5A\x61-\x7A"),
                CharacterClass::Hexadecimal => buf.extend_from_slice(b"\x30-\x39\x41-\x46\x61-\x66"),
                CharacterClass::NotLetter => buf.extend_from_slice(b"\x00-\x40\x5B-\x60\x7B-\xFF"),
                CharacterClass::NotControlCharacter => buf.extend_from_slice(b"\x20-\x7E\x80-\xFF"),
                CharacterClass::NotDigit => buf.extend_from_slice(b"\x00-\x2F\x3A-\xFF"),
                CharacterClass::NotPrintable => buf.extend_from_slice(b"\x00-\x20\x7F-\xFF"),
                CharacterClass::NotLowercase => buf.extend_from_slice(b"\x00-\x60\x7B-\xFF"),
                CharacterClass::NotPunctuation => buf.extend_from_slice(b"\x00-\x20\x30-\x39\x41-\x5A\x61-\x7A\x7F-\xFF"),
                CharacterClass::NotSpace => buf.extend_from_slice(b"\x00-\x08\x0E-\x1F\x21-\xFF"),
                CharacterClass::NotUppercase => buf.extend_from_slice(b"\x00-\x40\x5B-\xFF"),
                CharacterClass::NotAlphanumeric => buf.extend_from_slice(b"\x00-\x2F\x3A-\x40\x5B-\x60\x7B-\xFF"),
                CharacterClass::NotHexadecimal => buf.extend_from_slice(b"\x00-\x2F\x3A-\x40\x47-\x60\x67-\xFF"),
            }
            Ok(())
        }
    }

    fn parse_character_class(i: &[u8]) -> IResult<&[u8], CharacterClass> {
        alt((
            alt((
                map(tag("."), |_| CharacterClass::All),
                map(tag("%a"), |_| CharacterClass::Letter),
                map(tag("%c"), |_| CharacterClass::ControlCharacter),
                map(tag("%d"), |_| CharacterClass::Digit),
                map(tag("%g"), |_| CharacterClass::Printable),
                map(tag("%l"), |_| CharacterClass::Lowercase),
                map(tag("%p"), |_| CharacterClass::Punctuation),
                map(tag("%s"), |_| CharacterClass::Space),
                map(tag("%u"), |_| CharacterClass::Uppercase),
                map(tag("%w"), |_| CharacterClass::Alphanumeric),
                map(tag("%x"), |_| CharacterClass::Hexadecimal),
                map(tag("%A"), |_| CharacterClass::NotLetter),
                map(tag("%C"), |_| CharacterClass::NotControlCharacter),
                map(tag("%D"), |_| CharacterClass::NotDigit),
                map(tag("%G"), |_| CharacterClass::NotPrintable),
                map(tag("%L"), |_| CharacterClass::NotLowercase),
                map(tag("%P"), |_| CharacterClass::NotPunctuation),
                map(tag("%S"), |_| CharacterClass::NotSpace),
                map(tag("%U"), |_| CharacterClass::NotUppercase),
                map(tag("%W"), |_| CharacterClass::NotAlphanumeric),
                map(tag("%X"), |_| CharacterClass::NotHexadecimal),
            )),
            alt((
                map(preceded(tag("%"), none_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")), |c| CharacterClass::Character(c as u8)),
                map(none_of("^$()%.[]*+-?"), |c| CharacterClass::Character(c as u8)),
            ))
        ))(i)
    }

    #[derive(Debug)]
    enum SetComponent {
        CharacterClass(CharacterClass),
        Range { start: u8, end: u8 },
    }

    impl PatternComponent for SetComponent {
        fn compile(&self, buf: &mut Vec<u8>) -> Result<(), ArgumentError> {
            match self {
                SetComponent::CharacterClass(c) => c.compile(buf)?,
                SetComponent::Range { start, end } => {
                    SetComponent::escape_set(*start, buf);
                    buf.push('-' as u8);
                    SetComponent::escape_set(*end, buf);
                }
            }
            Ok(())
        }
    }

    fn parse_set_component(i: &[u8]) -> IResult<&[u8], SetComponent> {
        alt((
            map(tuple((
                take(1usize),
                tag("-"),
                take(1usize)
            )), |parts: (&[u8], &[u8], &[u8])| SetComponent::Range { start: parts.0[0], end: parts.2[0] }),
            map(parse_character_class, |c| SetComponent::CharacterClass(c))
        ))(i)
    }

    type PatternSet = Vec<SetComponent>;

    impl PatternComponent for PatternSet {
        fn compile(&self, buf: &mut Vec<u8>) -> Result<(), ArgumentError> {
            for setcomponent in self {
                setcomponent.compile(buf)?
            }
            Ok(())
        }
    }

    fn parse_pattern_set(i: &[u8]) -> IResult<&[u8], PatternSet> {
        many1(parse_set_component)(i)
    }

    #[derive(Debug)]
    enum CharacterClassOrSet {
        CharacterClass(CharacterClass),
        Set(PatternSet),
        ComplementSet(PatternSet),
    }

    impl PatternComponent for CharacterClassOrSet {
        fn compile(&self, buf: &mut Vec<u8>) -> Result<(), ArgumentError> {
            match self {
                CharacterClassOrSet::CharacterClass(c) => {
                    buf.push('[' as u8);
                    c.compile(buf)?;
                    buf.push(']' as u8);
                }
                CharacterClassOrSet::Set(s) => {
                    buf.push('[' as u8);
                    s.compile(buf)?;
                    buf.push(']' as u8);
                }
                CharacterClassOrSet::ComplementSet(s) => {
                    buf.push('[' as u8);
                    buf.push('^' as u8);
                    s.compile(buf)?;
                    buf.push(']' as u8);
                }
            }
            Ok(())
        }
    }

    fn parse_character_class_or_set(i: &[u8]) -> IResult<&[u8], CharacterClassOrSet> {
        alt((
            map(tuple((
                tag("^["),
                parse_pattern_set,
                tag("]")
            )), |tup| CharacterClassOrSet::ComplementSet(tup.1)),
            map(tuple((
                tag("["),
                parse_pattern_set,
                tag("]")
            )), |tup| CharacterClassOrSet::Set(tup.1)),
            map(parse_character_class, |c| CharacterClassOrSet::CharacterClass(c))
        ))(i)
    }

    #[derive(Debug)]
    enum Quantifier {
        ZeroOrOne,
        ZeroOrMore,
        OneOrMore,
        ZeroOrMoreLazy,
    }

    impl PatternComponent for Quantifier {
        fn compile(&self, buf: &mut Vec<u8>) -> Result<(), ArgumentError> {
            buf.extend_from_slice(match self {
                Quantifier::ZeroOrOne => b"?",
                Quantifier::ZeroOrMore => b"*",
                Quantifier::OneOrMore => b"+",
                Quantifier::ZeroOrMoreLazy => b"*?",
            });
            Ok(())
        }
    }

    named!(parse_quantifier<&[u8], Quantifier>,
        alt!(
            map!(tag!("?"), |_| Quantifier::ZeroOrOne) |
            map!(tag!("*"), |_| Quantifier::ZeroOrMore) |
            map!(tag!("+"), |_| Quantifier::OneOrMore) |
            map!(tag!("-"), |_| Quantifier::ZeroOrMoreLazy)
        )
    );

    #[derive(Debug)]
    enum PatternItem {
        SingleClass(CharacterClassOrSet),
        QuantifiedClass(CharacterClassOrSet, Quantifier),
        // CaptureIndex(u8),
        Balanced { left: u8, right: u8 },
        Frontier(PatternSet),
    }

    impl PatternComponent for PatternItem {
        fn compile(&self, buf: &mut Vec<u8>) -> Result<(), ArgumentError> {
            match self {
                PatternItem::SingleClass(c) => c.compile(buf)?,
                PatternItem::QuantifiedClass(c, q) => {
                    c.compile(buf)?;
                    q.compile(buf)?;
                }
                // PatternItem::CaptureIndex(i) => {}
                PatternItem::Balanced { .. } => return Err(ArgumentError::InvalidPatternFeature { message: "Balanced characters are unsupported!" }),
                PatternItem::Frontier(_) => return Err(ArgumentError::InvalidPatternFeature { message: "Frontier sets are unsupported!" }),
            }
            Ok(())
        }
    }

    named!(parse_pattern_item<&[u8], PatternItem>,
        alt!(
            // complete!(map!(preceded!(tag!("%"), one_of!("123456789")), |s| PatternItem::CaptureIndex(s as u8))) |
            complete!(map!(preceded!(tag!("%b"), tuple!(take!(1), take!(1))), |(left, right)| PatternItem::Balanced{ left: left[0], right: right[0] })) |
            complete!(map!(preceded!(tag!("%f"), parse_pattern_set), |s| PatternItem::Frontier(s))) |
            complete!(map!(tuple!(parse_character_class_or_set, parse_quantifier), |(c, q)| PatternItem::QuantifiedClass(c, q))) |
            complete!(map!(parse_character_class_or_set, |c| PatternItem::SingleClass(c)))
        )
    );

    #[derive(Debug)]
    pub struct Pattern {
        anchored_at_start: bool,
        sequence: Vec<PatternItem>,
        anchored_at_end: bool,
    }

    impl PatternComponent for Pattern {
        fn compile(&self, buf: &mut Vec<u8>) -> Result<(), ArgumentError> {
            buf.extend_from_slice(b"(?m-u)");
            if self.anchored_at_start { buf.push('^' as u8) }
            for item in &self.sequence {
                item.compile(buf)?;
            }
            if self.anchored_at_end { buf.push('$' as u8) }
            Ok(())
        }
    }

    // TODO: Rewrite with functions
    named!(parse_pattern<&[u8], Pattern>,
        complete!(map!(tuple!(opt!(complete!(char!('^'))), many0!(parse_pattern_item), opt!(complete!(char!('$')))),
            |(start, sequence, end)| Pattern { anchored_at_start: start.is_some(), sequence, anchored_at_end: end.is_some() }
        ))
    );

    pub fn compile_pattern(bytes: &[u8]) -> Result<Regex, ArgumentError> {  // TODO: Capture groups .-.
        let pattern = parse_pattern(bytes);
        match pattern {
            Ok((_, pattern)) => {
                let mut regex: Vec<u8> = Vec::new();
                pattern.compile(&mut regex)?;
                match Regex::new(std::str::from_utf8(&regex[..]).expect("Compiled regex is not UTF-8!")) {
                    Ok(r) => Ok(r),
                    Err(err) => Err(ArgumentError::InvalidPatternOrFormat { message: format!("{}", err) }),
                }
            }
            Result::Err(err) => Err(ArgumentError::InvalidPatternOrFormat { message: format!("{}", err) })
        }
    }
}
