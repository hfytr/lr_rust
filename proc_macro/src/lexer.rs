use std::collections::BTreeMap;

use shared_structs::{Conflict, ParseTable, Trie, RegexDFA, TrieNode};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    Error, ExprClosure, Ident, LitStr, Token,
};

use crate::Context;

const ERR_INCORRECT_TOKEN_SPEC: &'static str = r#"Provide each token in the form
    "Literal(<pattern>) or "Regex(<pattern>)"
    where pattern is a single string literal"#;
const ERR_INCORRECT_PROD: &'static str = r#"Each parsing rule should be of the form:
    RULE_NAME => Rule(<component 1> <component 2> ... <component n>) <optional callback>"#;
const ERR_MISSING_PROD_TYPE: &'static str =
    "Expected Literal | Regex | Rule after => in new production definition.";
const ERR_MISSING_PATT: &'static str =
    "Expected parenthesized string pattern after Regex | Literal.";
const ERR_PROD_NO_OUT_TYPE: &'static str = "Every production must start with ProductionName =>";
const ERR_NO_CALLBACK: &'static str =
    "Literal | Regex require a callback to be specified after the pattern";
const ERR_NO_ERR_CALLBACK: &'static str = "Error rules specified with ! must have a callback";
const ERR_UPDATE_RULE: &'static str = "Rules specified with _ => cannot be Rule()'s.";

pub enum Production {
    Lexeme {
        name: Ident,
        name_raw: String,
        patt: String,
        callback: ExprClosure,
        literal: bool,
    },
    Rule {
        name: Ident,
        name_raw: String,
        rules: Vec<(Vec<Ident>, Option<ExprClosure>)>,
        error: Option<ExprClosure>,
    },
    Update {
        patt: String,
        callback: ExprClosure,
        literal: bool,
    },
    None {
        name: Ident,
        name_raw: String,
    },
}

impl Production {
    pub fn get_name(&self) -> Option<(&Ident, &str)> {
        match self {
            Production::Lexeme { name_raw, name, .. } => Some((name, name_raw.as_str())),
            Production::Rule { name_raw, name, .. } => Some((name, name_raw.as_str())),
            Production::None { name_raw, name, .. } => Some((name, name_raw.as_str())),
            Production::Update { .. } => None,
        }
    }
}

impl Parse for Production {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: Option<Ident> = if input.parse::<Token![_]>().is_err() {
            input.parse().context(ERR_PROD_NO_OUT_TYPE)?
        } else {
            None
        };
        if input.peek(Token![,])
            && let Some(name) = name
        {
            return Ok(Production::None {
                name_raw: name.to_string(),
                name,
            });
        }
        input.parse::<Token![=>]>().context(ERR_PROD_NO_OUT_TYPE)?;
        let prod_type_raw = input.parse::<Ident>().context(ERR_MISSING_PROD_TYPE)?;
        let content;
        parenthesized!(content in input);
        let prod_type_str = prod_type_raw.to_string();
        match prod_type_str.as_str() {
            "Literal" | "Regex" => {
                let patt = content.parse::<LitStr>().context(ERR_MISSING_PATT)?.value();
                let callback = content.parse::<ExprClosure>().context(ERR_NO_CALLBACK)?;
                let literal = prod_type_str.chars().next().unwrap() == 'L';
                if let Some(name) = name {
                    Ok(Production::Lexeme {
                        name_raw: name.to_string(),
                        name: name,
                        patt,
                        callback,
                        literal,
                    })
                } else {
                    Ok(Production::Update {
                        patt,
                        callback,
                        literal,
                    })
                }
            }
            "Rule" => {
                let mut rules = vec![];
                let mut error = None;
                let mut last_was_ident = false;
                loop {
                    if let Result::Ok(ident) = content.parse::<Ident>() {
                        if !last_was_ident {
                            rules.push((vec![], None));
                        }
                        rules.last_mut().unwrap().0.push(ident);
                        last_was_ident = true;
                    } else if content.parse::<Token![!]>().is_ok() {
                        error = Some(content.parse().context(ERR_NO_ERR_CALLBACK)?)
                    } else if content.parse::<Token![,]>().is_ok() {
                        last_was_ident = false;
                    } else if last_was_ident
                        && let Result::Ok(callback) = content.parse::<ExprClosure>()
                    {
                        rules.last_mut().unwrap().1 = Some(callback);
                        last_was_ident = false;
                    } else if !content.is_empty() {
                        return Result::Err(Error::new(content.span(), ERR_INCORRECT_PROD));
                    } else {
                        break;
                    }
                }
                let name = name.expect(ERR_UPDATE_RULE);
                Ok(Production::Rule {
                    name_raw: name.to_string(),
                    name,
                    rules,
                    error,
                })
            }
            _ => Err(Error::new(prod_type_raw.span(), ERR_INCORRECT_TOKEN_SPEC)),
        }
    }
}

pub fn process_productions(
    productions: &Vec<Production>,
) -> (RegexDFA, Trie, ParseTable, Vec<bool>) {
    let mut trie = Trie(vec![TrieNode {
        fin: None,
        children: [None; 256],
    }]);
    fn update_ids<'a>(production_ids: &mut BTreeMap<&'a str, usize>, production: &'a str) -> usize {
        production_ids
            .get(production)
            .map(|x| *x)
            .unwrap_or_else(|| {
                production_ids.insert(production, production_ids.len());
                production_ids.len() - 1
            })
    }
    let (regexi, production_ids, mut is_token, _) = productions.into_iter().fold(
        (vec![], BTreeMap::new(), vec![false; productions.len()], 0),
        |(mut regexi, mut production_ids, mut is_token, mut lexeme_i), production| {
            match &production {
                Production::Lexeme {
                    patt,
                    name_raw,
                    literal,
                    ..
                } => {
                    is_token[update_ids(&mut production_ids, name_raw.as_str())] = true;
                    if *literal {
                        trie.insert(patt.as_bytes(), lexeme_i);
                    } else {
                        regexi.push((patt.clone(), lexeme_i));
                    }
                    lexeme_i += 1;
                }
                Production::Update { patt, literal, .. } => {
                    if *literal {
                        trie.insert(patt.as_bytes(), lexeme_i);
                    } else {
                        regexi.push((patt.clone(), lexeme_i));
                    }
                    lexeme_i += 1;
                }
                Production::Rule { name_raw, .. } => {
                    is_token[production_ids.len()] = false;
                    production_ids.insert(name_raw.as_str(), production_ids.len());
                }
                Production::None { name_raw, .. } => {
                    let production_id = update_ids(&mut production_ids, name_raw.as_str());
                    is_token[production_id] = true;
                }
            }
            (regexi, production_ids, is_token, lexeme_i)
        },
    );
    is_token.resize(production_ids.len(), false);

    let mut any_errors = false;
    // augment with eof token
    let mut rules: Vec<Vec<Vec<usize>>> = vec![vec![]; production_ids.len() + 1];
    let mut error_ids: Vec<Option<usize>> = vec![None; production_ids.len() + 1];
    let mut num_errors = 0usize;
    for (raw_components, nonterminal, error) in productions
        .iter()
        .filter_map(|prod| match &prod {
            Production::Rule {
                rules,
                name_raw,
                error,
                ..
            } => Some((rules, name_raw, error)),
            _ => None,
        })
        .flat_map(|(raw_components, rule_name, error)| {
            raw_components
                .iter()
                .map(move |raw_components| (raw_components, rule_name, error))
        })
    {
        let production_id = *production_ids.get(nonterminal.as_str()).unwrap();
        if error.is_some() {
            error_ids[production_id] = Some(num_errors);
            num_errors += 1;
        }
        rules[production_id].push(raw_components.0.iter().map(|raw_component|
            *production_ids.get(&raw_component.to_string().as_str()).unwrap_or_else(|| {
                eprintln!(r#"Reference to undefined production "{raw_component}" in definition of production "{nonterminal}""#);
                any_errors = true;
                &0
            })
        ).collect());
    }
    if any_errors {
        panic!();
    }
    let regex = RegexDFA::from_regexi(regexi.into_iter());
    let eprint_conflict = |node: usize, rule, item: usize, s| {
        let item_name = productions
            .get(item)
            .map(|p| p.get_name().map(|n| n.1))
            .unwrap_or(Some("EOF"))
            .unwrap_or("<Update>");
        eprintln!(
            "ERROR: {s} / Reduce conflict on item {} in rule {rule} of production {}",
            item_name,
            productions[node]
                .get_name()
                .map(|n| n.1)
                .unwrap_or("<Update>")
        )
    };
    let parser = match ParseTable::from_rules(rules, error_ids) {
        Err(conflicts) => {
            for conflict in conflicts {
                match conflict {
                    Conflict::RR(node, rule, item) => eprint_conflict(node, rule, item, "Reduce"),
                    Conflict::SR(node, rule, item) => eprint_conflict(node, rule, item, "Shift"),
                }
            }
            panic!();
        }
        Ok(parser) => parser,
    };

    (regex, trie, parser, is_token) // + 1 for eof
}
