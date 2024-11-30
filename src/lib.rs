use bluer_uuid::UuidExt;
use uuid::Uuid;

// Enum type for every SDP tag that exists
#[derive(Clone, Debug)]
pub enum Tag {
    Nil(),
    Boolean(bool),
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    UInt128(u128),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Int128(i128),
    Uuid(Uuid),
    Text(String),
    RawText(Vec<u8>),
    Url(String),
    Sequence(Vec<Tag>),
    Attribute(u16, Box<Tag>),
    Record(Vec<Tag>),
}

impl Tag {
    pub fn nil() -> Self {
        Tag::Nil()
    }
}

macro_rules! impl_from_trivial {
    ($t:ident $sdp:ident) => {
        impl From<$t> for Tag {
            fn from(value: $t) -> Self {
                Self::$sdp(value)
            }
        }
    };
}

impl_from_trivial!(bool Boolean);
impl_from_trivial!(u8 UInt8);
impl_from_trivial!(u16 UInt16);
impl_from_trivial!(u32 UInt32);
impl_from_trivial!(u64 UInt64);
impl_from_trivial!(u128 UInt128);
impl_from_trivial!(i8 Int8);
impl_from_trivial!(i16 Int16);
impl_from_trivial!(i32 Int32);
impl_from_trivial!(i64 Int64);
impl_from_trivial!(i128 Int128);

impl_from_trivial!(String Text);

/// Convert copyable reference to tag.
impl <R: Copy + Into<Tag>> From<&R> for Tag {
    fn from(value: &R) -> Self {
        (*value).into()
    }
}

/// Implemenetation to convert UUID to Tag
impl From<Uuid> for Tag {
    fn from(value: Uuid) -> Self {
        Self::Uuid(value)
    }
}

impl From<&str> for Tag {
    /// Convert string slice to Tag.
    fn from(value: &str) -> Self {
        Tag::Text(value.into())
    }
}


impl Tag {
    /// Convert byte sequences to Tag
    pub fn bytes(value: &[u8]) -> Self {
        Tag::RawText(value.into_iter().map(|e| *e).collect())
    }
}

impl Tag {
    /// Convert URL to Tag
    pub fn url(value: &str) -> Self {
        Tag::Url(value.to_string())
    }
    
    /// Convert sequence to Tag
    pub fn sequence<I: IntoIterator>(value: I) -> Self
    where I::Item: Into<Tag> {
        // Convert all elements to Tag, then collect into a vector.
        Tag::Sequence(value.into_iter().map(|e| e.into()).collect())
    }

    /// Create attribute SDP tag.
    pub fn attribute<C: Into<Tag>>(id: u16, child: C) -> Self {
        Tag::Attribute(id, Box::new(child.into()))
    }

    /// Create record SDP tag
    pub fn record<T: IntoIterator>(attributes: T) -> Self
    where T::Item: Into<Tag> {
        Tag::Record(attributes
                       .into_iter()
                       .map(|e| e.into())
                       .collect())
    }
}

//// Convert tuples to Tag

macro_rules! impl_from_tuple {
    ( $( $n:ident $t:ident ),+ ) => {
        impl <$($t: Into<Tag>),+> From<($($t,)+)> for Tag {
            fn from(($($n,)+): ($($t,)+)) -> Self {
                // Convert all elements in value to Tag.
                let converted = vec!($(($n).into()),+);
                Tag::Sequence(converted)
            }
        }
    }
}

impl_from_tuple!(a A);
impl_from_tuple!(a A, b B);
impl_from_tuple!(a A, b B, c C);
impl_from_tuple!(a A, b B, c C, d D);
impl_from_tuple!(a A, b B, c C, d D, e E);
impl_from_tuple!(a A, b B, c C, d D, e E, f F);
impl_from_tuple!(a A, b B, c C, d D, e E, f F, g G);
impl_from_tuple!(a A, b B, c C, d D, e E, f F, g G, h H);
impl_from_tuple!(a A, b B, c C, d D, e E, f F, g G, h H, i I);
impl_from_tuple!(a A, b B, c C, d D, e E, f F, g G, h H, i I, j J);
impl_from_tuple!(a A, b B, c C, d D, e E, f F, g G, h H, i I, j J, k K);
impl_from_tuple!(a A, b B, c C, d D, e E, f F, g G, h H, i I, j J, k K, l L);

//// Other containers

// Implementation to convert slice to Tag
impl <'a, E: 'a> From<&'a [E]> for Tag
where &'a E: Into<Tag> {
    fn from(value: &'a [E]) -> Self {
        Tag::Sequence(value.into_iter().map(|e| e.into()).collect())
    }
}

// Implementation to convert sized array to Tag
impl <E: Into<Tag>, const N: usize> From<[E; N]> for Tag {
    fn from(value: [E; N]) -> Self {
        Tag::Sequence(value.into_iter().map(|e| e.into()).collect())
    }
}


//// Conversion to XML

/// Given a UUID, format it one of three ways: a single byte hex value (0x12), a two-byte hex value
/// (0x1234), a four-byte hex value (0x12345678), or a full 128-bit UUID, such as
/// 00001234-0000-1000-8000-00805f9b34fb.
fn format_uuid(uuid: &Uuid) -> String {
    if let Some(uuid16) = uuid.as_u16() {
        if let Ok(uuid8) = u8::try_from(uuid16) {
            format!("0x{:02x}", uuid8)
        } else {
            format!("0x{:04x}", uuid16)
        }
    } else if let Some(uuid32) = uuid.as_u32() {
        format!("0x{:08x}", uuid32)
    } else {
        format!("{}", uuid.as_hyphenated())
    }
}

impl Tag {
    /// Return the name of the tag
    pub fn name(&self) -> &'static str {
        match self {
            Tag::Nil() => "nil",
            Tag::Boolean(..) => "boolean",
            Tag::UInt8(..) => "uint8",
            Tag::UInt16(..) => "uint16",
            Tag::UInt32(..) => "uint32",
            Tag::UInt64(..) => "uint64",
            Tag::UInt128(..) => "uint128",
            Tag::Int8(..) => "int8",
            Tag::Int16(..) => "int16",
            Tag::Int32(..) => "int32",
            Tag::Int64(..) => "int64",
            Tag::Int128(..) => "int128",
            Tag::Uuid(..) => "uuid",
            Tag::Text(..) => "text",
            Tag::RawText(..) => "text",
            Tag::Url(..) => "url",
            Tag::Sequence(..) => "sequence",
            Tag::Attribute(..) => "attribute",
            Tag::Record(..) => "record",
        }
    }

    /// Convert an SDP tag to an XML string.
    pub fn to_xml(&self) -> String {
        match self {
            Tag::Nil() => "<nil />".to_string(),
            Tag::Boolean(value) => format!("<boolean value=\"{}\" />",
                                         if *value { "true" } else { "false" }),
            Tag::UInt8(value) => format!("<uint8 value=\"0x{:02x}\" />", &value),
            Tag::UInt16(value) => format!("<uint16 value=\"0x{:04x}\" />", &value),
            Tag::UInt32(value) => format!("<uint32 value=\"0x{:08x}\" />", &value),
            Tag::UInt64(value) => format!("<uint64 value=\"0x{:016x}\" />", &value),
            Tag::UInt128(value) => format!("<uint128 value=\"{:032x}\" />", &value),
            Tag::Int8(value) => format!("<int8 value=\"0x{}\" />", &value),
            Tag::Int16(value) => format!("<int16 value=\"0x{}\" />", &value),
            Tag::Int32(value) => format!("<int32 value=\"0x{}\" />", &value),
            Tag::Int64(value) => format!("<int16 value=\"0x{}\" />", &value),
            Tag::Int128(value) => format!("<int128 value=\"{:032x}\" />", &value),
            Tag::Uuid(value) => format!("<uuid value=\"{}\" />", format_uuid(value)),
            Tag::Text(value) => {
                // Escape XML characters
                let escaped = value.replace("&", "&amp;")
                    .replace("<", "&lt;")
                    .replace(">", "&gt;")
                    .replace("\"", "&quot;");
                format!("<text value=\"{}\" />", escaped)
            },
            Tag::RawText(value) => {
                // Convert all bytes to hex and concatenate.
                let data = value.into_iter()
                    .map(|e| format!("{:02x}", e))
                    .collect::<Vec<String>>()
                    .join("");
                format!("<text encoding=\"hex\" value=\"{}\" />", &data)
            },
            Tag::Url(value) => format!("<url value=\"{}\" />", &value),
            Tag::Sequence(value) => {
                // Convert all children to XML and join the results.
                let children = value
                    .into_iter()
                    .map(|e| e.to_xml())
                    .collect::<Vec<String>>()
                    .join("");
                format!("<sequence>{}</sequence>", children)
            },
            Tag::Attribute(id, child) => {
                format!("<attribute id=\"0x{:04x}\">{}</attribute>",
                        id,
                        child.to_xml())
            },
            Tag::Record(children) => {
                let contents = children.into_iter()
                    .map(|e| e.to_xml())
                    .collect::<Vec<String>>()
                    .join("");
                format!("<record>{}</record>", contents)
            },
        }
    }

    pub fn to_xml_document(&self) -> String {
        format!("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>{}", self.to_xml())
    }
}

