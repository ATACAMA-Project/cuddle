use crate::deserialize::peek::peek;
use crate::encoding::{MajorType, MinorType};
use crate::serialize::owned::OwnedValue;
use crate::serialize::WriteTo;

pub fn array(bytes: &[u8]) -> Option<OwnedValue> {
    let major = MajorType::from(bytes);

    match major {
        MajorType::Array(minor) => {
            let len = match minor {
                MinorType::SameByte(x) => x as usize,
                MinorType::OneByte(x) => x as usize,
                MinorType::TwoBytes(x) => x as usize,
                MinorType::FourBytes(x) => x as usize,
                MinorType::EightBytes(x) => x as usize,
                _ => return None,
            };

            let mut v = Vec::new();
            let mut offset = major.len() as usize;
            for _ in 0..len {
                // Overflow?
                if offset >= bytes.len() {
                    return None;
                }

                let maybe_item = peek(&bytes[offset..]);
                match maybe_item {
                    Some(s) => {
                        offset += s.len();
                        v.push(s);
                    }
                    None => {
                        return None;
                    }
                }
            }
            Some(OwnedValue::from_array(v))
        }
        _ => None,
    }
}

pub fn indefinite_array(bytes: &[u8]) -> Option<OwnedValue> {
    let major = MajorType::from(bytes);

    match major {
        MajorType::Array(MinorType::Indefinite()) => {
            let mut v = Vec::new();
            // Remove the major type.
            let mut bytes = &bytes[1..];

            loop {
                eprintln!("bytes: {:?}", bytes);
                // Overflow?
                if bytes.is_empty() {
                    return None;
                }

                let subvalue = peek(&bytes);
                eprintln!("subvalue: {:?}", subvalue);
                match subvalue {
                    Some(s) if s.is_break() => {
                        return Some(OwnedValue::from_indefinite_array(v));
                    }
                    Some(s) => {
                        bytes = &bytes[s.len()..];
                        v.push(s);
                    }
                    None => {
                        return None;
                    }
                }
            }
        }
        _ => None,
    }
}
