use crate::attr::{self, Attrs};
use crate::generics::ParamsInScope;
use proc_macro2::Span;
use syn::{
    Data, DataEnum, DataStruct, DeriveInput, Error, Fields, Generics, Ident, Index, Member, Result,
    Type,
};

pub enum Input<'a> {
    Struct(Struct<'a>),
    Enum(Enum<'a>),
}

pub struct Struct<'a> {
    pub attrs: Attrs<'a>,
    pub ident: Ident,
    pub generics: &'a Generics,
    pub fields: Vec<Field<'a>>,
}

pub struct Enum<'a> {
    pub attrs: Attrs<'a>,
    pub ident: Ident,
    pub generics: &'a Generics,
    pub variants: Vec<Variant<'a>>,
}

pub struct Variant<'a> {
    pub original: &'a syn::Variant,
    pub attrs: Attrs<'a>,
    pub ident: Ident,
    pub fields: Vec<Field<'a>>,
}

pub struct Field<'a> {
    pub original: &'a syn::Field,
    pub attrs: Attrs<'a>,
    pub member: Member,
    pub ty: &'a Type,
    pub contains_generic: bool,
}

impl<'a> Input<'a> {
    pub fn from_syn(
        node: &'a DeriveInput,
        helpers: &'a crate::expand::DummyHelpers,
    ) -> Result<Self> {
        match &node.data {
            Data::Struct(data) => Struct::from_syn(node, data).map(Input::Struct),
            Data::Enum(data) => Enum::from_syn(node, data, helpers).map(Input::Enum),
            Data::Union(_) => Err(Error::new_spanned(
                node,
                "union as errors are not supported",
            )),
        }
    }
}

impl<'a> Struct<'a> {
    fn from_syn(node: &'a DeriveInput, data: &'a DataStruct) -> Result<Self> {
        let mut attrs = attr::get(&node.attrs)?;
        let scope = ParamsInScope::new(&node.generics);
        let span = attrs.span().unwrap_or_else(Span::call_site);
        let fields = Field::multiple_from_syn(&data.fields, &scope, span)?;
        if let Some(display) = &mut attrs.display {
            display.expand_shorthand(&fields);
        }
        Ok(Struct {
            attrs,
            ident: node.ident.clone(),
            generics: &node.generics,
            fields,
        })
    }
}

impl<'a> Enum<'a> {
    fn from_syn(
        node: &'a DeriveInput,
        data: &'a DataEnum,
        helpers: &'a crate::expand::DummyHelpers,
    ) -> Result<Self> {
        let attrs = attr::get(&node.attrs)?;
        let scope = ParamsInScope::new(&node.generics);
        let span = attrs.span().unwrap_or_else(Span::call_site);
        let variants = data
            .variants
            .iter()
            .map(|node| {
                let mut variant = Variant::from_syn(node, &scope, span, helpers)?;
                if let display @ None = &mut variant.attrs.display {
                    display.clone_from(&attrs.display);
                }
                if let Some(display) = &mut variant.attrs.display {
                    display.expand_shorthand(&variant.fields);
                } else if variant.attrs.transparent.is_none() {
                    variant.attrs.transparent = attrs.transparent;
                }
                Ok(variant)
            })
            .collect::<Result<_>>()?;
        Ok(Enum {
            attrs,
            ident: node.ident.clone(),
            generics: &node.generics,
            variants,
        })
    }
}

// const yoyo: syn::Attribute = syn::parse_quote!(#[dummy_attribute]);

#[allow(unused)]
fn dummy_parse_buffer<'a>() -> syn::parse::ParseBuffer<'a> {
    todo!()
}

#[allow(unused)]
fn dummy_attr() -> &'static syn::Attribute {
    // static cell: Arc<RwLock<syn::Attribute>> = Arc::new();
    let _a: syn::Attribute = syn::parse_quote!(#[dummy_attribute]);
    // let b: &'static syn::Attribute = &a;
    // let parsed: syn::parse::ParseStream = syn::parse_str("").unwrap();
    let _ts = proc_macro2::TokenStream::new();
    let pb: syn::parse::ParseBuffer = dummy_parse_buffer();
    let ps = syn::parse::ParseStream::from(&pb);
    let attr_all = ps.call(syn::Attribute::parse_outer).unwrap();
    attr_all.get(0).as_ref().unwrap();
    todo!()
}

impl<'a> Variant<'a> {
    fn from_syn(
        node: &'a syn::Variant,
        scope: &ParamsInScope<'a>,
        span: Span,
        helpers: &'a crate::expand::DummyHelpers,
    ) -> Result<Self> {
        let attrs = attr::get(&node.attrs)?;
        let span = attrs.span().unwrap_or(span);
        let mut variant = Variant {
            original: node,
            attrs,
            ident: node.ident.clone(),
            fields: Field::multiple_from_syn(&node.fields, scope, span)?,
        };
        if let Some(_dis) = variant.attrs.display.as_mut() {
        } else {
            if false {
                let dis = attr::Display {
                    original: &helpers.attr_empty,
                    fmt: syn::LitStr::new("dummy", span),
                    args: proc_macro2::TokenStream::new(),
                    has_bonus_display: false,
                    implied_bounds: std::collections::BTreeSet::new(),
                    requires_fmt_machinery: true,
                };
                variant.attrs.display = Some(dis);
            }
        }
        Ok(variant)
    }
}

impl<'a> Field<'a> {
    fn multiple_from_syn(
        fields: &'a Fields,
        scope: &ParamsInScope<'a>,
        span: Span,
    ) -> Result<Vec<Self>> {
        fields
            .iter()
            .enumerate()
            .map(|(i, field)| Field::from_syn(i, field, scope, span))
            .collect()
    }

    fn from_syn(
        i: usize,
        node: &'a syn::Field,
        scope: &ParamsInScope<'a>,
        span: Span,
    ) -> Result<Self> {
        let field = Field {
            original: node,
            attrs: attr::get(&node.attrs)?,
            member: node.ident.clone().map(Member::Named).unwrap_or_else(|| {
                Member::Unnamed(Index {
                    index: i as u32,
                    span,
                })
            }),
            ty: &node.ty,
            contains_generic: scope.intersects(&node.ty),
        };
        Ok(field)
    }
}

impl Attrs<'_> {
    pub fn span(&self) -> Option<Span> {
        if let Some(display) = &self.display {
            Some(display.fmt.span())
        } else if let Some(transparent) = &self.transparent {
            Some(transparent.span)
        } else {
            None
        }
    }
}
