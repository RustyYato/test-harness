use std::fmt::{Display, Write};

use serde::Serializer;

pub fn to_string(value: impl serde::Serialize) -> String {
    let mut ser = TestSerializer {
        value: TabbedString {
            value: String::new(),
            current_tabs: 0,
            next_tabs: 0,
        },
    };

    value.serialize(&mut ser).unwrap_or_else(|inf| match inf {});

    ser.value.value
}

struct TestSerializer {
    value: TabbedString,
}

struct TabbedString {
    value: String,
    current_tabs: u32,
    next_tabs: u32,
}

impl TabbedString {
    fn tab(&mut self) {
        for _ in 0..core::mem::take(&mut self.current_tabs) {
            self.value.push_str("  ");
        }
    }

    fn dec_tab(&mut self) {
        for _ in 1..core::mem::take(&mut self.current_tabs) {
            self.value.push_str("  ");
        }
    }

    fn push_str(&mut self, s: &str) {
        if s == "\n" && self.value.ends_with("\n") {
            return;
        }
        if s.contains(['(', '[', '{']) {
            self.next_tabs += 1;
        }
        if s.contains([')', ']', '}']) {
            self.next_tabs -= 1;
        }
        if s.contains('\n') {
            self.current_tabs = self.next_tabs;
        }
        self.value.push_str(s);
    }

    fn write_fmt(&mut self, args: core::fmt::Arguments<'_>) {
        let _ = self.value.write_fmt(args);
    }
}

#[derive(Debug)]
enum NeverError {}

impl std::error::Error for NeverError {}
impl core::fmt::Display for NeverError {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {}
    }
}

impl serde::ser::Error for NeverError {
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        panic!("never error: {msg}")
    }
}

impl Serializer for &mut TestSerializer {
    type Ok = ();
    type Error = NeverError;

    type SerializeSeq = Self;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;

    type SerializeStruct = Self;
    type SerializeStructVariant = Self;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        if v {
            self.value.push_str("true")
        } else {
            self.value.push_str("false")
        }
        Ok(())
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        write!(self.value, "{v:?}");
        Ok(())
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        write!(self.value, "{v:?}");
        Ok(())
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        write!(self.value, "{v:?}");
        Ok(())
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        write!(self.value, "{v:?}");
        Ok(())
    }

    fn serialize_i128(self, v: i128) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        write!(self.value, "{v:?}");
        Ok(())
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        write!(self.value, "{v:?}");
        Ok(())
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        write!(self.value, "{v:?}");
        Ok(())
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        write!(self.value, "{v:?}");
        Ok(())
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        write!(self.value, "{v:?}");
        Ok(())
    }

    fn serialize_u128(self, v: u128) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        write!(self.value, "{v:?}");
        Ok(())
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.serialize_f64(v.into())
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        write!(
            self.value,
            "{:3.12}",
            float_pretty_print::PrettyPrintFloat(v)
        );
        Ok(())
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        write!(self.value, "{v:?}");
        Ok(())
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        write!(self.value, "{v:?}");
        Ok(())
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        write!(self.value, "{v:?}");
        Ok(())
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit_variant("Option", 0, "None")
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        self.value.push_str("()");
        Ok(())
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        self.value.push_str(name);
        Ok(())
    }

    fn serialize_unit_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        self.value.tab();
        self.value.push_str(name);
        self.value.push_str("::");
        self.value.push_str(variant);
        Ok(())
    }

    fn serialize_newtype_struct<T>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.value.tab();
        self.value.push_str(name);
        self.value.push_str("(");
        value.serialize(&mut *self)?;
        self.value.dec_tab();
        self.value.push_str(")");
        Ok(())
    }

    fn serialize_newtype_variant<T>(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.value.tab();
        self.value.push_str(name);
        self.value.push_str("::");
        self.value.push_str(variant);
        self.value.push_str("(");
        value.serialize(&mut *self)?;
        self.value.dec_tab();
        self.value.push_str(")\n");
        Ok(())
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        self.value.tab();
        self.value.push_str("[\n");
        Ok(self)
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        self.value.tab();
        self.value.push_str("(\n");
        Ok(self)
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        self.value.tab();
        self.value.push_str(name);
        self.value.push_str("(\n");
        Ok(self)
    }

    fn serialize_tuple_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        self.value.tab();
        self.value.push_str(name);
        self.value.push_str("::");
        self.value.push_str(variant);
        self.value.push_str("(\n");
        Ok(self)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        self.value.tab();
        self.value.push_str("{");
        Ok(self)
    }

    fn serialize_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.value.tab();
        self.value.push_str(name);
        self.value.push_str(" {\n");
        Ok(self)
    }

    fn serialize_struct_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        self.value.tab();
        self.value.push_str(name);
        self.value.push_str("::");
        self.value.push_str(variant);
        self.value.push_str(" {\n");
        Ok(self)
    }
}

impl serde::ser::SerializeSeq for &mut TestSerializer {
    type Ok = ();
    type Error = NeverError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.value.tab();
        value.serialize(&mut **self)?;
        self.value.push_str("\n");
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.value.dec_tab();
        self.value.push_str("]\n");
        Ok(())
    }
}

impl serde::ser::SerializeTuple for &mut TestSerializer {
    type Ok = ();
    type Error = NeverError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.value.tab();
        value.serialize(&mut **self)?;
        self.value.push_str("\n");
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.value.dec_tab();
        self.value.push_str(")\n");
        Ok(())
    }
}

impl serde::ser::SerializeMap for &mut TestSerializer {
    type Ok = ();
    type Error = NeverError;

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.value.dec_tab();
        self.value.push_str("}\n");
        Ok(())
    }

    fn serialize_key<T>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.value.tab();
        key.serialize(&mut **self)?;
        self.value.push_str(": ");
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.value.tab();
        value.serialize(&mut **self)?;
        self.value.push_str("\n");
        Ok(())
    }
}

impl serde::ser::SerializeTupleStruct for &mut TestSerializer {
    type Ok = ();
    type Error = NeverError;

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.value.dec_tab();
        self.value.push_str(")\n");
        Ok(())
    }

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.value.tab();
        value.serialize(&mut **self)?;
        self.value.push_str("\n");
        Ok(())
    }
}

impl serde::ser::SerializeStruct for &mut TestSerializer {
    type Ok = ();
    type Error = NeverError;

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.value.dec_tab();
        self.value.push_str("}\n");
        Ok(())
    }

    fn serialize_field<T>(&mut self, name: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.value.tab();
        self.value.push_str(name);
        self.value.push_str(": ");
        value.serialize(&mut **self)?;
        self.value.push_str("\n");
        Ok(())
    }
}

impl serde::ser::SerializeStructVariant for &mut TestSerializer {
    type Ok = ();
    type Error = NeverError;

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.value.dec_tab();
        self.value.push_str("}\n");
        Ok(())
    }

    fn serialize_field<T>(&mut self, name: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.value.tab();
        self.value.push_str(name);
        self.value.push_str(": ");
        value.serialize(&mut **self)?;
        self.value.push_str("\n");
        Ok(())
    }
}

impl serde::ser::SerializeTupleVariant for &mut TestSerializer {
    type Ok = ();
    type Error = NeverError;

    fn end(self) -> Result<Self::Ok, Self::Error> {
        self.value.dec_tab();
        self.value.push_str(")\n");
        Ok(())
    }

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.value.tab();
        value.serialize(&mut **self)?;
        self.value.push_str("\n");
        Ok(())
    }
}
