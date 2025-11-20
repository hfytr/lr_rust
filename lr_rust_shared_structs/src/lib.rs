mod lexer;
mod parser;
mod sets;

use core::panic;
pub use lexer::{RegexDFA, Trie, TrieNode};
pub use parser::{Conflict, ParseAction, ParseTable};
use proc_macro2::TokenStream;
use quote::{ToTokens, quote};
use std::{
    fmt::Debug,
    io::{BufWriter, Write},
};

use crate::lexer::RegexTable;

const ERR_INVALID_LEXEME: &'static str = "The lexing engine hit an invalid sequence.";
const ERR_REDUCED_NONTERMINAL_INVALID: &'static str =
    "Non-terminal mapped to a non-goto action in the parse-table.";
const ERR_STATE_STACK_EMPTY: &'static str = "Attempted to pop from an empty state stack.";
const ERR_WRONG_OUTPUT: &'static str = "Parsing resulted in the wrong node type.";
const ERR_NODE_STACK_NOT_EMPTY: &'static str =
    "The node stack was not empty when returning start rule.";
const ERR_TERMINAL_GOTO: &'static str = "A terminal mapped to a goto action in the parse table.";
const ERR_SYNTAX_ERR: &'static str = "Syntax error.";
const ERR_OPEN_LOG: &'static str = "Error opening log file, \"lr_rust.log\".";
const ERR_IO: &'static str = "Error writing to log file, \"lr_rust.log\".";

fn quote_option<T: ToTokens>(o: &Option<T>) -> TokenStream {
    if let Some(t) = o {
        quote! { Some(#t) }
    } else {
        quote! { None }
    }
}

fn fmt_maybe_arr(f: &mut std::fmt::Formatter<'_>, a: &[Option<usize>; 256]) -> std::fmt::Result {
    f.write_str("[")?;
    for (i, e) in a
        .iter()
        .enumerate()
        .filter_map(|(i, maybe_e)| maybe_e.map(|e| (i, e)))
    {
        write!(f, "{}: {:?}, ", i, e)?
    }
    f.write_str("]")
}

pub enum NodeKind {
    Terminal,
    NonTerminal,
    Empty,
}

pub struct Engine<N: Clone, St, Sp> {
    parser: ParseTable,
    trie: Trie,
    lexer: RegexTable,
    id_productions: Vec<&'static str>,
    lexeme_callbacks: Vec<fn(&mut St, &str) -> Option<(N, Sp, usize)>>,
    error_callbacks: Vec<fn(&mut St, Vec<(N, Sp)>) -> (N, Sp)>,
    rule_callbacks: Vec<fn(&mut St, &mut Vec<(N, Sp)>) -> (N, Sp)>,
}

impl<N: Clone + Debug, St: Debug, Sp: Debug> Engine<N, St, Sp> {
    pub fn from_raw(
        parser: (
            Vec<Vec<(usize, usize)>>,
            Vec<(usize, usize)>,
            Vec<Option<(usize, usize)>>,
            Vec<Vec<bool>>,
            Vec<Option<usize>>,
        ),
        lexer: (Vec<[Option<usize>; 256]>, Vec<Option<usize>>),
        trie: Vec<(Option<usize>, [Option<usize>; 256])>,
        lexeme_callbacks: Vec<fn(&mut St, &str) -> Option<(N, Sp, usize)>>,
        error_callbacks: Vec<fn(&mut St, Vec<(N, Sp)>) -> (N, Sp)>,
        rule_callbacks: Vec<fn(&mut St, &mut Vec<(N, Sp)>) -> (N, Sp)>,
        id_productions: Vec<&'static str>,
    ) -> Result<Self, &'static str> {
        Ok(Self {
            parser: ParseTable::from_raw(parser.0, parser.1, parser.2, parser.3, parser.4)?,
            trie: Trie::from_raw(trie),
            lexer: RegexTable {
                trans: lexer.0,
                fin: lexer.1,
            },
            lexeme_callbacks,
            error_callbacks,
            rule_callbacks,
            id_productions,
        })
    }

    pub fn parse(
        &mut self,
        node: impl Into<usize>,
        mut s: &str,
        lex_state: &mut St,
    ) -> Result<N, &'static str> {
        let node: usize = node.into();
        let mut cur_lexeme = self.lex(&mut s, lex_state);
        if let Ok((Some(_), lexeme_id)) = cur_lexeme.as_ref()
            && node == *lexeme_id
        {
            return Ok(cur_lexeme.unwrap().0.unwrap().0);
        } else if self.parser.node_to_state[node].is_none() {
            return Err(ERR_SYNTAX_ERR);
        }
        let mut state_stack: Vec<usize> = vec![];
        state_stack.push(node);
        let mut node_stack = vec![];
        let mut error: Option<usize> = None;
        let mut result = None;
        let mut last_type = None;
        let log_file = std::fs::File::options()
            .create(true)
            .append(true)
            .open("lr_rust.log")
            .map_err(|_| ERR_OPEN_LOG)?;
        let mut writer = BufWriter::new(log_file);
        writeln!(writer, "-- PARSER ACTIONS --").map_err(|_| ERR_IO)?;
        while let Ok((lexeme, lexeme_id)) = cur_lexeme.as_ref() {
            writeln!(&mut writer, "State stack: {:?}", state_stack).map_err(|_| ERR_IO)?;
            writeln!(
                &mut writer,
                "Looking at token {}",
                self.id_productions[*lexeme_id]
            )
            .map_err(|_| ERR_IO)?;
            if let Some(nonterminal) = error {
                if self.parser.reductions[nonterminal][*lexeme_id] {
                    error = None;
                    let cur_state = *state_stack.last().unwrap();
                    if let ParseAction::Goto(state) = self.parser.actions[cur_state][*lexeme_id] {
                        state_stack.push(state);
                    } else {
                        panic!()
                    }
                } else {
                    cur_lexeme = self.lex(&mut s, lex_state);
                }
                continue;
            }
            match self.parser.actions[*state_stack.last().unwrap()][*lexeme_id] {
                ParseAction::Shift(state) => {
                    writeln!(&mut writer, "Shifted state {}", state).map_err(|_| ERR_IO)?;
                    state_stack.push(state);
                    if let Ok((Some((lexeme, span)), _)) = cur_lexeme {
                        node_stack.push((lexeme, span));
                    }
                    cur_lexeme = self.lex(&mut s, lex_state);
                }
                ParseAction::Reduce(rule) => {
                    let (rule_len, non_terminal) = self.parser.rule_lens[rule];
                    writeln!(
                        &mut writer,
                        "Reducing {} tokens to {}.",
                        rule_len, self.id_productions[non_terminal]
                    )
                    .map_err(|_| ERR_IO)?;
                    last_type = Some(non_terminal);
                    for _ in 0..rule_len {
                        state_stack.pop().ok_or(ERR_STATE_STACK_EMPTY)?;
                    }
                    let (new_node, new_span) =
                        (self.rule_callbacks[rule])(lex_state, &mut node_stack);
                    if s.is_empty() && non_terminal == node {
                        result = Some(node_stack.len());
                    }
                    node_stack.push((new_node, new_span));
                    state_stack.push(
                        if let ParseAction::Goto(state) = self.parser.actions
                            [*state_stack.last().ok_or(ERR_STATE_STACK_EMPTY)?][non_terminal]
                        {
                            state
                        } else if non_terminal == node && lexeme.is_none() {
                            break;
                        } else {
                            return Result::Err(ERR_REDUCED_NONTERMINAL_INVALID);
                        },
                    );
                }
                ParseAction::Invalid => {
                    writeln!(
                        &mut writer,
                        "Syntax Error. Final State stack: {:?}",
                        state_stack
                    )
                    .map_err(|_| ERR_IO)?;
                    if error.is_none() && result.is_none() {
                        writer.flush().map_err(|_| ERR_IO)?;
                        return Err(ERR_SYNTAX_ERR);
                    } else if error.is_none() && result.is_some() {
                        break;
                    }
                    let mut nodes = vec![];
                    let mut err_callback = None;
                    while let Some(state) = state_stack.last()
                        && !node_stack.is_empty()
                    {
                        if let Some((error_id, nonterminal)) = self.parser.errors[*state] {
                            error = Some(nonterminal);
                            err_callback = Some(error_id);
                            break;
                        }
                        nodes.push(node_stack.pop().unwrap());
                        state_stack.pop();
                    }
                    node_stack.push((self.error_callbacks[err_callback.unwrap()])(
                        lex_state, nodes,
                    ));
                }
                ParseAction::Goto(_) => return Result::Err(ERR_TERMINAL_GOTO),
            }
        }
        writer.flush().map_err(|_| ERR_IO)?;
        cur_lexeme?;
        if node_stack.len() != 1 {
            return Result::Err(ERR_NODE_STACK_NOT_EMPTY);
        } else if last_type != Some(node) {
            return Result::Err(ERR_WRONG_OUTPUT);
        } else {
            return Result::Ok(node_stack.pop().unwrap().0);
        }
    }

    pub fn lex(
        &self,
        s: &mut &str,
        state: &mut St,
    ) -> Result<(Option<(N, Sp)>, usize), &'static str> {
        let mut cur_lexeme = None;
        while cur_lexeme.is_none() {
            if s.is_empty() {
                return Ok((None, self.parser.actions[0].len() - 1));
            }
            let bytes = &s.as_bytes();
            let (trie_match, trie_len) = self.trie.query_longest(bytes);
            let (regex_match, regex_len) = self.lexer.query_longest(bytes);
            let (fin, len) = if trie_len > regex_len
                && let Some(fin) = trie_match
            {
                Ok((fin, trie_len))
            } else if regex_len > trie_len
                && let Some(fin) = regex_match
            {
                Ok((fin, regex_len))
            } else if regex_len == trie_len
                && let Some(regex_fin) = regex_match
                && let Some(trie_fin) = trie_match
            {
                if regex_fin < trie_fin {
                    Ok((regex_fin, regex_len))
                } else {
                    Ok((trie_fin, trie_len))
                }
            } else {
                Err(ERR_INVALID_LEXEME)
            }?;
            cur_lexeme = (self.lexeme_callbacks[fin])(state, &s[0..len]);
            *s = &s[len..];
        }
        let (lexeme, span, id) = cur_lexeme.unwrap();
        Ok((Some((lexeme, span)), id))
    }
}
