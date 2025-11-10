use super::fmt_maybe_arr;
use crate::{
    quote_option,
    sets::{IndexableMap, USizeSet},
};
use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use std::{collections::BTreeMap, fmt::Debug, rc::Rc};

#[derive(Clone, Copy)]
pub struct TrieNode {
    pub fin: Option<usize>,
    pub children: [Option<usize>; 256],
}

impl Debug for TrieNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fin.fmt(f)?;
        f.write_str("\n")?;
        fmt_maybe_arr(f, &self.children)
    }
}

impl TrieNode {
    pub fn from_raw(fin: Option<usize>, children: [Option<usize>; 256]) -> Self {
        Self { fin, children }
    }
}

impl ToTokens for TrieNode {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let fin = if let Some(lexeme_id) = self.fin {
            quote! { Some(#lexeme_id) }
        } else {
            quote! { None }
        };
        let mut children_inner = TokenStream::new();
        children_inner.append_separated(
            self.children.iter().map(quote_option),
            Punct::new(',', Spacing::Alone),
        );
        tokens.append_all(quote! {
            (#fin, [#children_inner])
        });
    }
}


#[derive(Debug)]
pub struct Trie(pub Vec<TrieNode>);

impl Trie {
    pub fn from_raw(trie_raw: Vec<(Option<usize>, [Option<usize>; 256])>) -> Self {
        let mut nodes = vec![TrieNode {
            fin: None,
            children: [None; 256],
        }; trie_raw.len()];
        for (i, (fin, children)) in trie_raw.into_iter().enumerate() {
            nodes[i] = TrieNode { fin, children };
        }
        Trie(nodes)
    }

    pub fn query_longest(&self, s: &[u8]) -> (Option<usize>, usize) {
        let mut cur_match = None;
        let mut match_len = 0;
        let mut cur = 0;
        let mut len = 0;
        while len < s.len()
            && let Some(next) = self.0[cur].children[s[len] as usize]
        {
            len += 1;
            if let Some(fin) = self.0[next].fin {
                cur_match = Some(fin);
                match_len = len;
            }
            cur = next;
        }
        (cur_match, match_len)
    }

    pub fn insert(&mut self, s: &[u8], x: usize) {
        let mut cur = 0;
        for c in s {
            cur = self.0[cur].children[*c as usize].unwrap_or_else(|| {
                self.0[cur].children[*c as usize] = Some(self.0.len());
                self.0.push(TrieNode {
                    fin: None,
                    children: [None; 256],
                });
                self.0.len() - 1
            });
        }
        self.0[cur].fin = Some(x)
    }
}

impl<const N: usize> From<[TrieNode; N]> for Trie {
    fn from(a: [TrieNode; N]) -> Self {
        Self(a.into_iter().collect())
    }
}

impl ToTokens for Trie {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut result = TokenStream::new();
        result.append_separated(self.0.iter(), Punct::new(',', Spacing::Alone));
        tokens.append_all(quote! { vec![#result] });
    }
}

const ESCAPE_ARRAY: [(u8, u8); 9] = [
    (b'n', b'\n'),
    (b't', b'\t'),
    (b'(', b'('),
    (b')', b')'),
    (b'[', b'['),
    (b']', b']'),
    (b'^', b'^'),
    (b'|', b'|'),
    (b'-', b'-'),
];

#[derive(Debug)]
pub struct Empty();

impl ToTokens for Empty {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(quote! { () });
    }
}

#[derive(Clone)]
pub struct RegexTable {
    pub trans: Vec<[Option<usize>; 256]>,
    pub fin: Vec<Option<usize>>,
}

impl Debug for RegexTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        RegexDFA::from_raw(self.clone()).fmt(f)
    }
}

impl RegexTable {
    pub fn query_longest(&self, s: &[u8]) -> (Option<usize>, usize) {
        let mut cur_match = None;
        let mut match_len = 0;
        let mut cur = 0;
        let mut len = 0;
        while len < s.len()
            && let Some(next) = self.trans[cur][s[len] as usize]
        {
            len += 1;
            if let Some(fin) = self.fin[next] {
                cur_match = Some(fin);
                match_len = len;
            }
            cur = next;
        }
        (cur_match, match_len)
    }
}

pub struct RegexDFA {
    pub states: IndexableMap<USizeSet, Empty>,
    pub trans: Vec<[Option<usize>; 256]>,
    pub fin: Vec<Option<usize>>,
}

impl Debug for RegexDFA {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        println!("{:?}", '\n');
        write!(f, "RegexTable {{")?;
        for (i, tran) in self.trans.iter().enumerate() {
            if let Some(fin) = self.fin[i] {
                write!(f, "\n    State {i} ({fin}): ")?;
            } else {
                write!(f, "\n    State {i}: ")?;
            }
            let mut cur = &None;
            let mut start = u32::MAX;
            for (j, state) in tran.iter().enumerate().map(|(j, s)| (j as u32, s)) {
                if state != cur && j != 0 {
                    if cur.is_some() {
                        let start_c = char::from_u32(start).unwrap();
                        let j_c = char::from_u32(j - 1).unwrap();
                        write!(f, "{:?}-{:?}: {}, ", start_c, j_c, cur.as_ref().unwrap())?;
                    }
                    cur = state;
                    start = j;
                }
            }
        }
        writeln!(f, "\n}}")?;
        Ok(())
    }
}

impl RegexDFA {
    pub fn from_regexi(regexi: impl Iterator<Item = (String, usize)>) -> Self {
        let nfa = NFA::from_regexi(regexi);
        let init_state = nfa.epsilon_closure(0);
        let mut res = Self {
            states: IndexableMap::from([(init_state, Empty())]),
            trans: vec![],
            fin: vec![None],
        };
        let mut stack = vec![0];
        while let Some(cur_ind) = stack.pop() {
            let cur_state = Rc::clone(&res.states[cur_ind].0);
            let mut char_state = [None; 256];
            for (edge, dest) in cur_state
                .iter()
                .flat_map(|node| nfa.edges[node].iter())
                .filter_map(|(edge, dest)| edge.map(|e| (e, dest)))
            {
                let mut new_state_map: BTreeMap<usize, usize> = BTreeMap::new();
                let new_state = nfa.epsilon_closure(*dest);
                let base_state_ind = res.states.get_ind(&new_state).unwrap_or_else(|| {
                    stack.push(res.states.len());
                    res.fin.push(
                        new_state
                            .iter()
                            .fold(None, |acc, nfa_state| acc.or(nfa.fin[nfa_state])),
                    );
                    res.states.push(new_state, Empty()).0
                });
                for i in (edge.0..=edge.1).map(usize::from) {
                    if let Some(old_state) = char_state[i] {
                        char_state[i] = new_state_map.get(&old_state).map(|x| *x).or_else(|| {
                            let new_state = res.states[old_state].0.as_ref()
                                | res.states[base_state_ind].0.as_ref();
                            let new_ind = res.states.get_ind(&new_state).unwrap_or_else(|| {
                                stack.push(res.states.len());
                                res.fin.push(new_state.iter().fold(None, |acc, nfa_state| {
                                    acc.and_then(|acc| nfa.fin[nfa_state].map(|cur| cur.max(acc)))
                                        .or(acc)
                                        .or(nfa.fin[nfa_state])
                                }));
                                res.states.push(new_state, Empty()).0
                            });
                            new_state_map.insert(old_state, new_ind);
                            Some(new_ind)
                        });
                    } else {
                        char_state[i] = Some(base_state_ind);
                    }
                }
            }
            if res.trans.len() < cur_ind + 1 {
                res.trans.resize(cur_ind + 1, [None; 256]);
            }
            res.trans[cur_ind] = char_state;
        }
        res
    }

    pub fn from_raw(raw: RegexTable) -> Self {
        Self {
            states: IndexableMap::from([]),
            trans: raw.trans.to_vec(),
            fin: raw.fin.to_vec(),
        }
    }
}

fn quote_option_vec<T, F: FnMut(&Option<T>) -> TokenStream>(
    callback: F,
    v: &[Option<T>],
) -> TokenStream {
    let mut res = TokenStream::new();
    res.append_separated(v.iter().map(callback), Punct::new(',', Spacing::Alone));
    res
}

impl ToTokens for RegexDFA {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut trans_inner = TokenStream::new();
        trans_inner.append_separated(
            self.trans.iter().map(|tran| {
                let tran_raw = quote_option_vec(quote_option, tran);
                quote! { [#tran_raw] }
            }),
            Punct::new(',', Spacing::Alone),
        );
        let fin_inner = quote_option_vec(
            |fin| {
                if let Some(lexeme_id) = fin {
                    quote! { Some(#lexeme_id) }
                } else {
                    quote! { None }
                }
            },
            &self.fin,
        );
        tokens.append_all(quote! {
            (vec![#trans_inner], vec![#fin_inner])
        });
    }
}

struct NFA {
    edges: Vec<Vec<(Option<(u8, u8)>, usize)>>,
    fin: Vec<Option<usize>>,
}

impl Debug for NFA {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        println!("{:?}", '\n');
        write!(f, "NFA {{")?;
        for (i, tran) in self.edges.iter().enumerate() {
            write!(f, "\n    State {i}: ")?;
            for (edge, state) in tran.iter() {
                if let Some((start, end)) = edge {
                    let start_c = char::from_u32(*start as u32).unwrap();
                    let end_c = char::from_u32(*end as u32).unwrap();
                    write!(f, "Some(({:?}, {:?})): {}, ", start_c, end_c, state)?;
                } else {
                    write!(f, "None: {state}, ")?;
                }
            }
        }
        writeln!(f, "\n}}")?;
        Ok(())
    }
}

impl NFA {
    pub fn epsilon_closure(&self, node: usize) -> USizeSet {
        let mut stack = vec![node];
        let mut res = USizeSet::default();
        let mut vis = USizeSet::default();
        vis.set(node, true);
        while let Some(cur) = stack.pop() {
            res.set(cur, true);
            stack.extend(
                self.edges[cur]
                    .iter()
                    .filter_map(|(maybe_e, v)| maybe_e.map(|_| 0).xor(Some(*v)))
                    .filter(|v| {
                        let res = !vis.get(*v);
                        vis.set(*v, true);
                        res
                    }),
            );
            self.edges[cur]
                .iter()
                .filter(|(e, _)| e.is_none())
                .for_each(|(_, to)| res.set(*to, true));
        }
        res
    }

    pub fn from_regexi(regexi: impl Iterator<Item = (String, usize)>) -> Self {
        let mut init = Self::from_regex("", 0).0;
        init.fin = vec![None];
        regexi.fold(init, |mut acc, regex| {
            let (mut new_nfa, new_fin) = Self::from_regex(&regex.0, acc.fin.len());
            let len = acc.edges.len();
            acc.fin.resize(len + new_nfa.edges.len(), None);
            acc.fin[new_fin] = Some(regex.1);
            acc.edges[0].push((None, len));
            acc.edges.append(&mut new_nfa.edges);
            acc
        })
    }

    fn from_regex(regex: &str, node_offset: usize) -> (Self, usize) {
        let mut escape_lookup = [None; 256];
        for (key, val) in ESCAPE_ARRAY {
            escape_lookup[key as usize] = Some(val);
        }
        let mut escaped = false;
        let mut groups = vec![];
        let mut sq = None;
        let mut sq_chars: [u64; 4] = [0; 4];
        let mut sq_not = false;
        let mut last = (node_offset, node_offset);
        let mut edges = vec![vec![]];
        let mut in_group = 0;

        let mut s = regex.as_bytes();
        while s.len() != 0 {
            match (sq, s) {
                (_, [b'\\']) if !escaped => panic!("ERROR: Last character is \\."),
                (_, [b'\\', tail @ ..]) if !escaped && tail.len() != 0 => {
                    escaped = true;
                    s = tail;
                }
                (_, [b'*', tail @ ..]) if !escaped => {
                    edges[last.0 - node_offset].push((None, last.1));
                    edges[last.1 - node_offset].push((None, last.0));
                    s = tail;
                }
                (None, [b'.', tail @ ..]) if !escaped => {
                    let new_node = edges.len() + node_offset;
                    edges[last.1 - node_offset].push((Some((u8::MIN, u8::MAX)), new_node));
                    edges.push(vec![]);
                    last = (last.1, new_node);
                    s = tail;
                }
                (Some(_), [b'-', tail @ ..]) if !escaped && tail.len() != 0 => {
                    panic!("ERROR: Leading -")
                }
                (Some(_), [b'-', tail @ ..]) if !escaped && tail.len() == 0 => {
                    panic!("ERROR: Trailing -")
                }
                (Some(_), [c0, b'-', c1, tail @ ..]) if *c0 != b'\\' => {
                    let mut c0 = *c0;
                    if escaped {
                        c0 = escape_lookup[c0 as usize].expect("ERROR; Invalid escape code.");
                    }
                    let mut c1 = *c1;
                    let mut tail = tail;
                    if c1 == b'\\' {
                        c1 = escape_lookup[tail[0] as usize].expect("ERROR: Invalid escape code.");
                        tail = &tail[1..]
                    }
                    for i in c0..=c1 {
                        sq_chars[i as usize / 64] |= 1 << (i % 64);
                    }
                    s = tail;
                }
                (Some(_), [b'(', ..]) => panic!("ERROR; () in []."),
                (_, [b'(', tail @ ..]) if !escaped => {
                    groups.push((last.1, edges.len() + node_offset));
                    in_group += 1;
                    edges.push(vec![]);
                    let new_node = edges.len() + node_offset;
                    edges[last.1 - node_offset].push((None, new_node));
                    last = (last.1, new_node);
                    edges.push(vec![]);
                    s = tail;
                }
                (_, [b')', ..]) if !escaped && in_group == 0 => panic!("ERROR: Trailing )."),
                (_, [b')', tail @ ..]) if !escaped && groups.len() != 0 => {
                    edges[last.1 - node_offset].push((None, groups.last().unwrap().1));
                    last = groups.pop().unwrap();
                    in_group -= 1;
                    s = tail;
                }
                (_, [b'|', ..]) if !escaped && in_group == 0 => panic!("ERROR: | not in ()."),
                (_, [b'|', tail @ ..]) if !escaped && in_group != 0 => {
                    edges[last.1 - node_offset].push((None, groups.last().unwrap().1));
                    last = (0, groups.last().unwrap().0);
                    let new_node = edges.len() + node_offset;
                    edges[last.1 - node_offset].push((None, new_node));
                    last = (last.1, new_node);
                    edges.push(vec![]);
                    s = tail;
                }
                (_, [b'[', tail @ ..]) if !escaped => {
                    sq = Some((last.1, edges.len() + node_offset));
                    last = (last.1, edges.len() + node_offset);
                    sq_chars = [0; 4];
                    edges.push(vec![]);
                    s = tail;
                }
                (None, [b']', ..]) if !escaped => panic!("ERROR: Trailing ]"),
                (Some((sq_start, sq_end)), [b']', tail @ ..]) if !escaped => {
                    let mut start = 0;
                    let mut end = 0;
                    let mut in_feasible = false;
                    if sq_not {
                        for i in 0..4 {
                            sq_chars[i] = !sq_chars[i];
                        }
                        sq_not = false;
                    }
                    let get_u8_set = |i: u8| sq_chars[i as usize / 64] & (1 << (i % 64)) > 0;
                    for i in 0..=255 {
                        if get_u8_set(i) && in_feasible {
                            end += 1;
                        } else if in_feasible {
                            edges[sq_start - node_offset].push((Some((start, end)), sq_end));
                            in_feasible = false;
                        } else if get_u8_set(i) {
                            in_feasible = true;
                            start = i;
                            end = i;
                        }
                    }
                    if in_feasible {
                        edges[sq_start - node_offset].push((Some((start, end)), sq_end));
                    }
                    sq = None;
                    s = tail;
                }
                (Some(_), [b'^', ..]) if !escaped && sq_not => panic!("ERROR: Multiple ^ in []"),
                (Some(_), [b'^', tail @ ..]) if !escaped && !sq_not => {
                    sq_not = true;
                    s = tail;
                }
                (Some(_), [c, tail @ ..]) => {
                    let mut c = *c;
                    if escaped {
                        c = escape_lookup[c as usize].unwrap_or(c);
                        escaped = false;
                    }
                    sq_chars[c as usize / 64] |= 1 << (c % 64);
                    s = tail;
                }
                (None, [c, tail @ ..]) => {
                    let mut c = *c;
                    if escaped {
                        c = escape_lookup[c as usize].unwrap_or(c);
                        escaped = false;
                    }
                    let new_node = edges.len() + node_offset;
                    edges[last.1 - node_offset].push((Some((c, c)), new_node));
                    last = (last.1, new_node);
                    edges.push(vec![]);
                    s = tail;
                }
                _ => {}
            }
        }
        let fin = vec![];
        (Self { edges, fin }, last.1)
    }
}
