use chumsky::prelude::*;
use serde::Serialize;
use std::collections::BTreeMap;
use text::TextParser;

#[derive(Clone, Debug, Serialize)]
pub enum NorgMeta {
    Invalid,
    Nil,
    Bool(bool),
    Str(String),
    EmptyKey(String),
    Num(f64),
    Array(Vec<NorgMeta>),
    Object(BTreeMap<String, NorgMeta>),
}

const SPECIAL: &str = "{}[]:\n";

pub fn meta_parser() -> impl Parser<char, NorgMeta, Error = Simple<char>> {
    recursive(|value| {
        let frac = just('.').chain(text::digits(10));

        let exp = just('e')
            .or(just('E'))
            .chain(just('+').or(just('-')).or_not())
            .chain::<char, _, _>(text::digits(10));

        let number = just(' ')
            .repeated()
            .ignore_then(just('-').or_not())
            .chain::<char, _, _>(text::int(10))
            .chain::<char, _, _>(frac.or_not().flatten())
            .chain::<char, _, _>(exp.or_not().flatten())
            .then_ignore(just('\n').rewind())
            .collect::<String>()
            .from_str()
            .unwrapped()
            .labelled("number");

        let escape = just('\\').ignore_then(
            just('\\')
                .or(just('/'))
                .or(one_of(SPECIAL))
                .or(just('b').to('\x08'))
                .or(just('f').to('\x0C'))
                .or(just('n').to('\n'))
                .or(just('r').to('\r'))
                .or(just('t').to('\t'))
                .or(just('u').ignore_then(
                    filter(|c: &char| c.is_ascii_hexdigit())
                        .repeated()
                        .exactly(4)
                        .collect::<String>()
                        .validate(|digits, span, emit| {
                            char::from_u32(u32::from_str_radix(&digits, 16).unwrap())
                                .unwrap_or_else(|| {
                                    emit(Simple::custom(span, "invalid unicode character"));
                                    '\u{FFFD}' // unicode replacement character
                                })
                        }),
                )),
        );

        let string = none_of("{}[]\n")
            .or(escape.clone())
            .repeated()
            .at_least(1)
            .try_map(|x, span| {
                let binding = x.clone().into_iter().collect::<String>();
                let s = binding.trim();
                if s.is_empty() {
                    Err(Simple::custom(
                        span,
                        format!("strings can't be all whitespace, got {x:?}"),
                    ))
                } else {
                    Ok(s.to_string())
                }
            })
            .map(|s| match &s[..] {
                "true" => NorgMeta::Bool(true),
                "false" => NorgMeta::Bool(false),
                "nil" => NorgMeta::Nil,
                _ => NorgMeta::Str(s),
            });

        let key = none_of(SPECIAL)
            .repeated()
            .at_least(1)
            .then_ignore(just(':').padded())
            .collect::<String>()
            .map(|s| s.trim().to_string())
            .labelled("key");

        let array = value
            .clone()
            .separated_by(just('\n'))
            .allow_trailing()
            .padded()
            .delimited_by(just('[').padded(), just(']').ignored())
            .map(NorgMeta::Array)
            .labelled("array");

        let empty_array = empty()
            .padded()
            .delimited_by(just('['), just(']'))
            .to(NorgMeta::Array(vec![]));

        let property = key
            .clone()
            .then_ignore(one_of(" \n\t").or_not())
            .then(value.or(empty().to(NorgMeta::Nil)))
            .then_ignore(just('\n').or_not())
            .labelled("property");

        let object = property
            .clone()
            .then_ignore(just('\n').or_not())
            .repeated()
            .padded()
            .collect()
            .delimited_by(just('{').padded(), just('}').ignored())
            .map(NorgMeta::Object)
            .labelled("object");

        choice((
            number.map(NorgMeta::Num),
            key.then_ignore(just('\n')).map(NorgMeta::EmptyKey),
            empty_array,
            array,
            object,
            string,
        ))
        .recover_with(nested_delimiters('{', '}', [('[', ']')], |_| {
            NorgMeta::Invalid
        }))
        .recover_with(nested_delimiters('[', ']', [('{', '}')], |_| {
            NorgMeta::Invalid
        }))
        .recover_with(skip_then_retry_until(['}', ']']))
    })
    .then_ignore(end().padded().recover_with(skip_then_retry_until([])))
}
