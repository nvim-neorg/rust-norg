use chumsky::prelude::*;
use serde::Serialize;
use std::collections::BTreeMap;

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

pub fn meta_parser<'a>() -> impl Parser<'a, &'a str, NorgMeta, extra::Err<Rich<'a, char>>> {
    recursive::<_, _, extra::Err<Rich<'a, char>>, _, _>(|value| {
        let number = just(' ')
            .repeated()
            .ignore_then(
                text::int(10)
                    .then(just('.').ignore_then(text::digits(10).to_slice().or_not()))
                    .then(
                        just('e')
                            .or(just('E'))
                            .then(just('+').or(just('-')).or_not())
                            .then(text::digits(10).to_slice())
                            .to_slice()
                            .or_not(),
                    )
                    .then_ignore(just('\n').rewind())
                    .try_map(|((negative, frac), exp), span| {
                        let mut s = String::new();
                        s.push_str(negative);
                        if let Some(frac) = frac {
                            s.push('.');
                            s.push_str(frac);
                        }
                        if let Some(exp) = exp {
                            s.push_str(exp);
                        }
                        s.parse::<f64>().map(NorgMeta::Num).map_err(|_| {
                            Rich::custom(span, "invalid number")
                        })
                    }),
            );

        let escape = just('\\').ignore_then(
            just('\\')
                .or(just('/'))
                .or(one_of(SPECIAL))
                .or(just('b').to('\x08'))
                .or(just('f').to('\x0C'))
                .or(just('n').to('\n'))
                .or(just('r').to('\r'))
                .or(just('t').to('\x09'))
                .or(just('u').ignore_then(
                    any::<_, extra::Err<Rich<char>>>()
                        .filter(|c: &char| c.is_ascii_hexdigit())
                        .repeated()
                        .exactly(4)
                        .collect::<String>()
                        .validate(|digits, extra, emit| {
                            let result = char::from_u32(u32::from_str_radix(&digits, 16).unwrap())
                                .unwrap_or_else(|| {
                                    emit.emit(Rich::custom(extra.span(), "invalid unicode character"));
                                    '\u{FFFD}'
                                });
                            result
                        }),
                )),
        );

        let string = none_of("{}[]\n")
            .or(escape)
            .repeated()
            .at_least(1)
            .collect::<String>()
            .try_map(|s, span| {
                let trimmed = s.trim();
                if trimmed.is_empty() {
                    Err(Rich::custom(
                        span,
                        format!("strings can't be all whitespace, got {:?}", s),
                    ))
                } else {
                    Ok(match &s[..] {
                        "true" => NorgMeta::Bool(true),
                        "false" => NorgMeta::Bool(false),
                        "nil" => NorgMeta::Nil,
                        _ => NorgMeta::Str(s),
                    })
                }
            });

        let key = none_of(SPECIAL)
            .repeated()
            .at_least(1)
            .collect::<String>()
            .then_ignore(just(':').then(one_of(" \t").repeated()))
            .map(|s| s.trim().to_string())
            .labelled("key");

        let array = value
            .clone()
            .separated_by(just('\n'))
            .allow_trailing()
            .collect::<Vec<_>>()
            .padded()
            .delimited_by(just('[').padded(), just(']').ignored())
            .map(NorgMeta::Array)
            .labelled("array");

        let empty_array = empty()
            .padded()
            .delimited_by(just('[').padded(), just(']'))
            .to(NorgMeta::Array(vec![]));

        let property = key
            .then_ignore(one_of(" \t").repeated())
            .then(value.or(empty().to(NorgMeta::Nil)))
            .then_ignore(just('\n').or_not())
            .labelled("property");

        let object = property
            .clone()
            .then_ignore(just('\n').or_not())
            .repeated()
            .collect::<Vec<_>>()
            .padded()
            .delimited_by(just('{').padded(), just('}').ignored())
            .map(|pairs: Vec<_>| NorgMeta::Object(pairs.into_iter().collect()))
            .labelled("object");

        choice((
            number,
            empty_array,
            array,
            object,
            string,
        ))
    })
    .then_ignore(end())
}