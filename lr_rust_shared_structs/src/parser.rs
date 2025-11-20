use std::fmt::Debug;
use std::io::Write;

use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{ToTokens, TokenStreamExt, quote};

use crate::{
    quote_option,
    sets::{IndexableMap, USizeSet},
};

const ERR_INVALID_PA_ID: &'static str = "Parse actions must have kind 0 / 1 / 2 / 3";

pub const PA_ID_SHIFT: usize = 0;
pub const PA_ID_GOTO: usize = 1;
pub const PA_ID_REDUCE: usize = 2;
pub const PA_ID_INVALID: usize = 3;

#[derive(Debug, Clone, Ord, PartialEq, PartialOrd, Eq)]
struct SeedRule {
    nt: usize,
    rule: usize,
    pos: usize,
}

#[derive(Clone, Copy)]
pub enum ParseAction {
    Shift(usize),
    Goto(usize),
    Reduce(usize),
    Invalid,
}

impl Debug for ParseAction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            ParseAction::Shift(state) => write!(f, "Shift({state})"),
            ParseAction::Goto(state) => write!(f, "Goto({state})"),
            ParseAction::Reduce(prod) => write!(f, "Reduce({prod})"),
            ParseAction::Invalid => write!(f, "Invalid"),
        }
    }
}

impl ToTokens for ParseAction {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let (pa_kind, pa_val) = match self {
            ParseAction::Shift(state) => (PA_ID_SHIFT, *state),
            ParseAction::Goto(state) => (PA_ID_GOTO, *state),
            ParseAction::Reduce(rule) => (PA_ID_REDUCE, *rule),
            ParseAction::Invalid => (PA_ID_INVALID, 0),
        };
        tokens.append_all(quote! { (#pa_kind, #pa_val) });
    }
}

#[derive(Debug)]
pub struct ParseTable {
    pub actions: Vec<Vec<ParseAction>>,
    pub errors: Vec<Option<(usize, usize)>>,
    pub reductions: Vec<Vec<bool>>,
    pub rule_lens: Vec<(usize, usize)>,
    pub node_to_state: Vec<Option<usize>>,
}

pub struct Conflict {
    state_id: usize,
    state_rules: Vec<SeedRule>,
    state_lookaheads: Vec<USizeSet>,
    /// Vec<(token id, true if shift / reduce)>
    tokens: Vec<(usize, bool)>,
}

impl Conflict {
    pub fn pprint(&self, id_to_node: &Vec<String>) {
        println!("Parser state {}:", self.state_id);
        for (rule, lookaheads) in self.state_rules.iter().zip(self.state_lookaheads.iter()) {
            println!(
                "\tPosition {} of rule {} of nonterminal {} with lookahead:",
                rule.pos, rule.rule, id_to_node[rule.nt]
            );
            for lookahead in lookaheads.iter() {
                println!("\t\t{}", id_to_node[lookahead]);
            }
        }
        println!("\tConflicts:");
        for (token, shift_reduce) in self.tokens.iter() {
            let kind_str = if *shift_reduce { "Shift" } else { "Reduce" };
            println!("\t\t{}: {} / Reduce", id_to_node[*token], kind_str);
        }
    }
}

impl ParseTable {
    pub fn from_raw(
        actions_raw: Vec<Vec<(usize, usize)>>,
        rule_lens: Vec<(usize, usize)>,
        errors: Vec<Option<(usize, usize)>>,
        reductions: Vec<Vec<bool>>,
        node_to_state: Vec<Option<usize>>,
    ) -> Result<Self, &'static str> {
        let mut actions = vec![vec![ParseAction::Invalid; actions_raw[0].len()]; actions_raw.len()];
        for (i, state_actions) in actions_raw.into_iter().enumerate() {
            for (j, (action_type, value)) in state_actions.into_iter().enumerate() {
                actions[i][j] = match action_type {
                    PA_ID_SHIFT => ParseAction::Shift(value),
                    PA_ID_GOTO => ParseAction::Goto(value),
                    PA_ID_REDUCE => ParseAction::Reduce(value),
                    PA_ID_INVALID => ParseAction::Invalid,
                    _ => return Err(ERR_INVALID_PA_ID),
                };
            }
        }
        Ok(Self {
            actions,
            rule_lens,
            errors,
            reductions,
            node_to_state,
        })
    }

    pub fn from_rules(
        rules: Vec<Vec<Vec<usize>>>,
        error_callbacks: Vec<Option<usize>>,
    ) -> (Result<Self, Vec<Conflict>>, ParseDFA) {
        let dfa = ParseDFA::from_rules(rules);
        let mut conflicts = vec![];
        #[cfg(not(feature = "lr1"))]
        let (state_ids, max_id) = {
            let mut found_seeds = std::collections::BTreeMap::<&Vec<SeedRule>, usize>::new();
            let mut state_ids = vec![usize::MAX; dfa.states.len()];
            for (i, (state, _)) in dfa.states.iter().enumerate() {
                state_ids[i] = found_seeds.get(&state.0).map(|i| *i).unwrap_or_else(|| {
                    found_seeds.insert(&state.0, found_seeds.len());
                    found_seeds.len() - 1
                });
            }
            (state_ids, found_seeds.len())
        };
        #[cfg(feature = "lr1")]
        let (state_ids, max_id) = ((0..dfa.states.len()).collect::<Vec<_>>(), dfa.states.len());
        let mut actions = vec![vec![ParseAction::Invalid; dfa.rules.len()]; max_id];
        let mut production_ids = vec![];
        let mut rule_lens = vec![];
        let mut i = 0;
        for (prod, prod_rules) in dfa.rules.iter().enumerate() {
            production_ids.push(vec![]);
            for rule in prod_rules.iter() {
                production_ids.last_mut().unwrap().push(i);
                rule_lens.push((rule.len(), prod));
                i += 1;
            }
        }
        let mut errors = vec![None; actions.len()];
        let mut reductions = vec![vec![false; dfa.rules.len()]; dfa.rules.len()];
        for (i, (state, trans)) in dfa.states.iter().enumerate() {
            let (seeds, lookaheads) = &**state;
            let id = state_ids[i];
            let mut maybe_conflict: Option<Conflict> = None;
            for (seed, lookahead) in seeds.into_iter().zip(lookaheads.into_iter()) {
                for (item, next) in trans {
                    if matches!(actions[id][*item], ParseAction::Reduce(_)) {
                        match maybe_conflict.as_mut() {
                            Some(conflict) => {
                                conflict.tokens.push((*item, true));
                            }
                            None => {
                                maybe_conflict = Some(Conflict {
                                    state_id: i,
                                    state_rules: state.0.clone(),
                                    state_lookaheads: state.1.clone(),
                                    tokens: vec![(*item, true)],
                                });
                            }
                        }
                    }
                    actions[id][*item] = if dfa.rules[*item].len() == 0 {
                        ParseAction::Shift(state_ids[*next])
                    } else {
                        if let Some(error) = error_callbacks[*item] {
                            errors[state_ids[*next]] = Some((error, *item));
                        }
                        ParseAction::Goto(state_ids[*next])
                    };
                }
                if seed.pos == dfa.rules[seed.nt][seed.rule].len() {
                    for item in lookahead.iter() {
                        let production = production_ids[seed.nt][seed.rule];
                        if let ParseAction::Reduce(old_prod) = actions[id][item]
                            && old_prod != production
                        {
                            match maybe_conflict.as_mut() {
                                Some(conflict) => {
                                    conflict.tokens.push((item, false));
                                }
                                None => {
                                    maybe_conflict = Some(Conflict {
                                        state_id: i,
                                        state_rules: state.0.clone(),
                                        state_lookaheads: state.1.clone(),
                                        tokens: vec![(item, false)],
                                    });
                                }
                            }
                        }
                        reductions[seed.nt][item] = true;
                        actions[id][item] = ParseAction::Reduce(production);
                    }
                }
            }
            if let Some(conflict) = maybe_conflict {
                conflicts.push(conflict);
            }
        }
        let parser = if conflicts.is_empty() {
            Ok(Self {
                actions,
                rule_lens,
                reductions,
                errors,
                node_to_state: dfa.node_to_state.clone(),
            })
        } else {
            Err(conflicts)
        };
        (parser, dfa)
    }
}

impl ToTokens for ParseTable {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        fn quote_2d<T: ToTokens>(a: &Vec<Vec<T>>) -> TokenStream {
            let mut result_inner = TokenStream::new();
            result_inner.append_separated(
                a.iter().map(|v| {
                    let mut row_inner = TokenStream::new();
                    row_inner.append_separated(v.iter(), Punct::new(',', Spacing::Alone));
                    quote! { vec![#row_inner] }
                }),
                Punct::new(',', Spacing::Alone),
            );
            quote! { vec![#result_inner] }
        }
        fn quote_1d<T, F: Fn(&T) -> TokenStream>(v: &Vec<T>, f: F) -> TokenStream {
            let mut result_inner = TokenStream::new();
            result_inner.append_separated(v.iter().map(f), Punct::new(',', Spacing::Alone));
            quote! { vec![#result_inner] }
        }
        let actions = quote_2d(&self.actions);
        let reductions = quote_2d(&self.reductions);
        let quote_pair = |(x, y): &(usize, usize)| quote! { (#x, #y) };
        let rule_lens = quote_1d(&self.rule_lens, quote_pair);
        let errors = quote_1d(&self.errors, |error| {
            quote_option(&error.map(|e| quote_pair(&e)))
        });
        let node_to_state = quote_1d(&self.node_to_state, quote_option);
        tokens.append_all(quote! { (#actions, #rule_lens, #errors, #reductions, #node_to_state) });
    }
}

#[derive(Debug)]
pub struct ParseDFA {
    states: IndexableMap<(Vec<SeedRule>, Vec<USizeSet>), Vec<(usize, usize)>>,
    rules: Vec<Vec<Vec<usize>>>,
    node_to_state: Vec<Option<usize>>,
}

// TODO: allow epsilon rules
fn get_firsts(rules: &Vec<Vec<Vec<usize>>>) -> Vec<USizeSet> {
    fn helper(
        firsts: &mut Vec<USizeSet>,
        vis: &mut Vec<bool>,
        rules: &Vec<Vec<Vec<usize>>>,
        cur: usize,
    ) {
        vis[cur] = true;
        if rules[cur].is_empty() {
            firsts[cur].set(cur, true);
            return;
        }
        for dep in rules[cur]
            .iter()
            .filter_map(|prod| prod.first().map(|x| *x))
            .filter(|dep| *dep != cur)
        {
            if !vis[dep] {
                helper(firsts, vis, rules, dep);
            }
            let [src, dst] = firsts.get_disjoint_mut([cur, dep]).unwrap();
            *src |= dst;
        }
    }

    let mut firsts: Vec<USizeSet> = vec![USizeSet::default(); rules.len()];
    let mut vis = vec![false; rules.len()];
    for node in 0..rules.len() {
        if !vis[node] {
            helper(&mut firsts, &mut vis, &rules, node);
        }
    }
    firsts
}

fn get_derived_rules(rules: &Vec<Vec<Vec<usize>>>, start: usize) -> Vec<(usize, usize)> {
    fn helper(
        cur: usize,
        result: &mut Vec<(usize, usize)>,
        vis: &mut Vec<bool>,
        rules: &Vec<Vec<Vec<usize>>>,
    ) {
        vis[cur] = true;
        for (i, rule) in rules[cur].iter().enumerate() {
            result.push((cur, i));
            if !vis[rule[0]] && !rules[rule[0]].is_empty() {
                helper(rule[0], result, vis, rules);
            }
        }
    }
    let mut result = vec![];
    let mut vis = vec![false; rules.len()];
    helper(start, &mut result, &mut vis, &rules);
    result
}

fn get_derived_lookaheads(
    seed: &SeedRule,
    seed_lookahead: &USizeSet,
    firsts: &Vec<USizeSet>,
    rules: &Vec<Vec<Vec<usize>>>,
    debug: bool,
) -> Vec<Option<USizeSet>> {
    fn helper(
        cur: usize,
        cur_lookahead: &USizeSet,
        firsts: &Vec<USizeSet>,
        result: &mut Vec<Option<USizeSet>>,
        rules: &Vec<Vec<Vec<usize>>>,
        debug: bool
    ) {
        if let Some(ref mut cur_result) = result[cur] {
            *cur_result |= cur_lookahead
        } else {
            result[cur] = Some(cur_lookahead.clone());
        }
        for rule in rules[cur].iter() {
            let next_lookahead = rule.get(1).map(|n| &firsts[*n]).unwrap_or(cur_lookahead);
            if result[rule[0]]
                .as_ref()
                .map(|next_result| !next_lookahead.is_subset(next_result))
                .unwrap_or(true)
                && rules[rule[0]].len() > 0
            {
                helper(rule[0], &next_lookahead, firsts, result, rules, debug);
            }
        }
    }
    let mut result = vec![None; rules.len()];
    let cur_lookahead = rules[seed.nt][seed.rule]
        .get(seed.pos + 1)
        .map(|n| &firsts[*n])
        .unwrap_or(seed_lookahead);
    helper(
        rules[seed.nt][seed.rule][seed.pos],
        cur_lookahead,
        firsts,
        &mut result,
        rules,
        debug,
    );
    result
}

impl ParseDFA {
    fn from_rules(rules: Vec<Vec<Vec<usize>>>) -> Self {
        let firsts = get_firsts(&rules);
        let node_to_state: Vec<_> = rules
            .iter()
            .scan(0usize, |state, rule| {
                if rule.is_empty() {
                    Some(None)
                } else {
                    *state += 1;
                    Some(Some(*state - 1))
                }
            })
            .collect();
        let states_vec = rules
            .iter()
            .enumerate()
            .filter(|(_, rule)| !rule.is_empty())
            .map(|(i, rule)| {
                (
                    (
                        (0..rule.len())
                            .map(|j| SeedRule {
                                nt: i,
                                rule: j,
                                pos: 0,
                            })
                            .collect::<Vec<_>>(),
                        vec![firsts.last().unwrap().clone(); rule.len()],
                    ),
                    vec![],
                )
            })
            .collect::<Vec<_>>();
        let mut stack: Vec<_> = node_to_state.iter().filter_map(|i| *i).collect();
        let mut states = IndexableMap::from(states_vec);
        let derived_rules = (0..rules.len())
            .map(|i| get_derived_rules(&rules, i))
            .collect::<Vec<_>>();

        while let Some(cur_state) = stack.pop() {
            // every non-reduce seed and its lookahead
            let iter_state = || {
                states[cur_state]
                    .0
                    .0
                    .iter()
                    .zip(states[cur_state].0.1.iter())
                    .filter(|(seed, _)| seed.pos < rules[seed.nt][seed.rule].len())
            };
            if iter_state().next().is_none() {
                continue;
            }
            let mut trans_seeds = vec![vec![]; rules.len()];
            let mut trans_lookaheads = vec![vec![]; rules.len()];
            for (seed, lookahead) in iter_state() {
                let new_seed = SeedRule {
                    nt: seed.nt,
                    rule: seed.rule,
                    pos: seed.pos + 1,
                };
                let edge = rules[seed.nt][seed.rule][seed.pos];
                // seeds dont overlap so neither do new seeds
                trans_seeds[edge].push(new_seed);
                trans_lookaheads[edge].push(lookahead.clone());
            }
            let derived_lookaheads = iter_state()
                .map(|(seed, lookahead)| get_derived_lookaheads(seed, &lookahead, &firsts, &rules, cur_state == 1422))
                .collect::<Vec<_>>();
            for (i, rule) in iter_state().enumerate().flat_map(|(i, (seed, _))| {
                derived_rules[rules[seed.nt][seed.rule][seed.pos]]
                    .iter()
                    .map(move |rule| (i, rule))
            }) {
                let new_seed = SeedRule {
                    nt: rule.0,
                    rule: rule.1,
                    pos: 1,
                };
                let edge = rules[rule.0][rule.1][0];
                if let Some((tran, _)) = trans_seeds[edge]
                    .iter()
                    .enumerate()
                    .filter(|(_, seed)| **seed == new_seed)
                    .next()
                {
                    trans_lookaheads[edge][tran] |= derived_lookaheads[i][rule.0].as_ref().unwrap();
                } else {
                    trans_seeds[edge].push(new_seed);
                    trans_lookaheads[edge].push(derived_lookaheads[i][rule.0].clone().unwrap());
                }
            }
            let true_tran = trans_seeds
                .into_iter()
                .zip(trans_lookaheads.into_iter())
                .enumerate()
                .filter_map(|(i, state)| {
                    (state.0.len() > 0)
                        .then(|| {
                            states.get_ind(&state).unwrap_or_else(|| {
                                let state_id = states.push(state, vec![]).0;
                                stack.push(state_id);
                                state_id
                            })
                        })
                        .map(|next| (i, next))
                })
                .collect();
            states[cur_state].1 = true_tran;
        }

        Self {
            rules,
            states,
            node_to_state,
        }
    }

    pub fn write_to_file(&self, f: &mut std::fs::File, id_productions: &Vec<String>) -> std::io::Result<()> {
        for (i, (seeds, trans)) in self.states.iter().enumerate() {
            let lookaheads = &seeds.1;
            let seeds = &seeds.0;
            writeln!(f, "-- STATE {} --", i)?;
            writeln!(f, "  state rules")?;
            for (seed, lookahead) in seeds.iter().zip(lookaheads.iter()) {
                write!(f, "    {} =>", id_productions[seed.nt])?;
                for (j, symbol) in self.rules[seed.nt][seed.rule].iter().enumerate() {
                    if j == seed.pos{
                        write!(f, " |")?;
                    }
                    write!(f, " {}", id_productions[*symbol])?;
                }
                write!(f, " [")?;
                for (j, symbol) in lookahead.iter().enumerate() {
                    if j != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", id_productions[symbol])?;
                }
                writeln!(f, "],")?;
            }
            writeln!(f, "  transitions")?;
            for (symbol, next) in trans.iter() {
                writeln!(f, "    {} => {}", id_productions[*symbol], next)?;
            }
            if trans.is_empty() {
                writeln!(f, "    None")?;
            }
            writeln!(f, "")?;
        }
        Ok(())
    }
}
