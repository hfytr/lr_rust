use std::fmt::Debug;

use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};

use crate::{
    quote_option,
    sets::{IndexableMap, USizeSet},
};

const ERR_INVALID_PA_ID: &'static str = "Parse actions must have kind 0 / 1 / 2 / 3";

pub const PA_ID_SHIFT: usize = 0;
pub const PA_ID_GOTO: usize = 1;
pub const PA_ID_REDUCE: usize = 2;
pub const PA_ID_INVALID: usize = 3;

type SeedRule = (usize, usize, usize);

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
}

pub enum Conflict {
    RR(usize, usize, usize),
    SR(usize, usize, usize),
}

impl ParseTable {
    pub fn from_raw(
        actions_raw: Vec<Vec<(usize, usize)>>,
        rule_lens: Vec<(usize, usize)>,
        errors: Vec<Option<(usize, usize)>>,
        reductions: Vec<Vec<bool>>,
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
        })
    }

    pub fn from_rules(
        rules: Vec<Vec<Vec<usize>>>,
        error_callbacks: Vec<Option<usize>>,
    ) -> Result<Self, Vec<Conflict>> {
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
            for (seed, lookahead) in seeds.into_iter().zip(lookaheads.into_iter()) {
                for (item, next) in trans
                    .iter()
                    .enumerate()
                    .filter_map(|(i, tran)| tran.map(move |tran| (i, tran)))
                {
                    if matches!(actions[id][item], ParseAction::Reduce(_)) {
                        conflicts.push(Conflict::SR(seed.0, seed.1, item));
                    }
                    actions[id][item] = if dfa.rules[item].len() == 0 {
                        ParseAction::Shift(state_ids[next])
                    } else {
                        if let Some(error) = error_callbacks[item] {
                            errors[state_ids[next]] = Some((error, item));
                        }
                        ParseAction::Goto(state_ids[next])
                    };
                }
                if seed.2 == dfa.rules[seed.0][seed.1].len() {
                    for item in lookahead.iter() {
                        let production = production_ids[seed.0][seed.1];
                        if let ParseAction::Reduce(old_prod) = actions[id][item]
                            && old_prod != production
                        {
                            conflicts.push(Conflict::RR(seed.0, seed.1, item));
                        }
                        reductions[seed.0][item] = true;
                        actions[id][item] = ParseAction::Reduce(production);
                    }
                }
            }
        }
        conflicts
            .is_empty()
            .then_some(Self {
                actions,
                rule_lens,
                reductions,
                errors,
            })
            .ok_or(conflicts)
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
        tokens.append_all(quote! { (#actions, #rule_lens, #errors, #reductions) });
    }
}

#[derive(Debug)]
struct ParseDFA {
    states: IndexableMap<(Vec<SeedRule>, Vec<USizeSet>), Vec<Option<usize>>>,
    rules: Vec<Vec<Vec<usize>>>,
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
    seed: SeedRule,
    seed_lookahead: &USizeSet,
    firsts: &Vec<USizeSet>,
    rules: &Vec<Vec<Vec<usize>>>,
) -> Vec<Option<USizeSet>> {
    fn helper(
        cur: usize,
        cur_lookahead: &USizeSet,
        firsts: &Vec<USizeSet>,
        result: &mut Vec<Option<USizeSet>>,
        rules: &Vec<Vec<Vec<usize>>>,
    ) {
        if let Some(ref mut cur_result) = result[cur] {
            *cur_result |= cur_lookahead
        } else {
            result[cur] = Some(cur_lookahead.clone());
        }
        for (i, rule) in rules[cur].iter().enumerate() {
            let next_lookahead = rule
                .get(i + 1)
                .map(|n| &firsts[*n])
                .unwrap_or(cur_lookahead);
            if result[rule[0]]
                .as_ref()
                .map(|next_result| !next_lookahead.is_subset(next_result))
                .unwrap_or(true)
                && rules[rule[0]].len() > 0
            {
                helper(rule[0], &next_lookahead, firsts, result, rules);
            }
        }
    }
    let mut result = vec![None; rules.len()];
    let cur_lookahead = rules[seed.0][seed.1]
        .get(seed.2 + 1)
        .map(|n| &firsts[*n])
        .unwrap_or(seed_lookahead);
    helper(
        rules[seed.0][seed.1][seed.2],
        cur_lookahead,
        firsts,
        &mut result,
        rules,
    );
    result
}

impl ParseDFA {
    fn from_rules(rules: Vec<Vec<Vec<usize>>>) -> Self {
        let firsts = get_firsts(&rules);
        let mut stack = (0..rules.len())
            .filter(|i| !rules[*i].is_empty())
            .collect::<Vec<_>>();
        let states_vec = stack
            .iter()
            .map(|i| {
                (
                    (
                        (0..rules[*i].len()).map(|j| (*i, j, 0)).collect::<Vec<_>>(),
                        vec![firsts.last().unwrap().clone(); rules[*i].len()],
                    ),
                    vec![],
                )
            })
            .collect::<Vec<_>>();
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
                    .zip(states[cur_state].0 .1.iter())
                    .filter(|(seed, _)| seed.2 < rules[seed.0][seed.1].len())
            };
            if iter_state().next().is_none() {
                continue;
            }
            let mut trans = vec![(vec![], vec![]); rules.len()];
            for (seed, lookahead) in iter_state() {
                let new_seed = (seed.0, seed.1, seed.2 + 1);
                let edge = rules[seed.0][seed.1][seed.2];
                // seeds dont overlap so neither do new seeds
                trans[edge].0.push(new_seed);
                trans[edge].1.push(lookahead.clone());
            }
            let derived_lookaheads = iter_state()
                .map(|(seed, lookahead)| get_derived_lookaheads(*seed, &lookahead, &firsts, &rules))
                .collect::<Vec<_>>();
            for (i, rule) in iter_state().enumerate().flat_map(|(i, (seed, _))| {
                derived_rules[rules[seed.0][seed.1][seed.2]]
                    .iter()
                    .map(move |rule| (i, rule))
            }) {
                let new_seed = (rule.0, rule.1, 1);
                let edge = rules[rule.0][rule.1][0];
                if let Some((tran, _)) = trans[edge]
                    .0
                    .iter()
                    .enumerate()
                    .filter(|(_, seed)| **seed == new_seed)
                    .next()
                {
                    trans[edge].1[tran] |= derived_lookaheads[i][rule.0].as_ref().unwrap();
                } else {
                    trans[edge].0.push(new_seed);
                    trans[edge]
                        .1
                        .push(derived_lookaheads[i][rule.0].clone().unwrap());
                }
            }
            let true_tran = trans
                .into_iter()
                .map(|state| {
                    (state.0.len() > 0).then(|| {
                        states.get_ind(&state).unwrap_or_else(|| {
                            let state_id = states.push(state, vec![]).0;
                            stack.push(state_id);
                            state_id
                        })
                    })
                })
                .collect();
            states[cur_state].1 = true_tran;
        }

        Self { rules, states }
    }
}
