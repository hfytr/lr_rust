use std::{
    collections::BTreeMap,
    fmt::Debug,
    ops::{BitOr, BitOrAssign, Index, IndexMut, Not},
    rc::Rc,
};

use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};

#[derive(Default, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct USizeSet(pub Vec<u64>);

impl Debug for USizeSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("USizeSet")?;
        write!(f, "{:?}", self.iter().collect::<Vec<_>>())
    }
}

impl<'a> USizeSet {
    pub fn set(&mut self, i: usize, b: bool) {
        if i / 64 >= self.0.len() {
            self.0.resize(i / 64 + 1, 0);
        }
        self.0[i / 64] &= !(1 << (i % 64));
        self.0[i / 64] |= (b as u64) << (i % 64);
        if self.0[i / 64] == 0 && i / 64 == self.0.len() - 1 {
            self.0.pop();
        }
    }
    pub fn get(&self, i: usize) -> bool {
        if i / 64 >= self.0.len() {
            return false;
        }
        self.0[i / 64] & (1 << (i % 64)) > 0
    }
    pub fn iter(&'a self) -> USizeSetIterator<'a> {
        USizeSetIterator {
            ind: self.0.len().checked_sub(1).unwrap_or(0),
            cur: *self.0.last().unwrap_or(&0),
            set: &self,
        }
    }
    pub fn is_subset(&self, rhs: &Self) -> bool {
        self.0.len() <= rhs.0.len() && self.0.iter().zip(rhs.0.iter()).all(|(l, r)| l & r == *l)
    }
    pub fn is_disjoint(&self, rhs: &Self) -> bool {
        self.0.iter().zip(rhs.0.iter()).all(|(l, r)| l & r == 0)
    }
}

impl<const N: usize> From<[u64; N]> for USizeSet {
    fn from(value: [u64; N]) -> Self {
        Self(value.to_vec())
    }
}

impl<const N: usize> From<[usize; N]> for USizeSet {
    fn from(value: [usize; N]) -> Self {
        value.into_iter().collect()
    }
}

impl FromIterator<usize> for USizeSet {
    fn from_iter<T: IntoIterator<Item = usize>>(iter: T) -> Self {
        let mut res = Self(vec![]);
        for x in iter {
            res.set(x, true);
        }
        res
    }
}

impl Not for USizeSet {
    type Output = Self;
    fn not(self) -> Self::Output {
        Self(self.0.iter().map(Not::not).collect())
    }
}

impl BitOrAssign<&USizeSet> for USizeSet {
    fn bitor_assign(&mut self, rhs: &Self) {
        let len = if rhs.0.len() > self.0.len() {
            let res = self.0.len();
            self.0.extend_from_slice(&rhs.0[res..]);
            res
        } else {
            rhs.0.len()
        };
        for i in 0..len {
            self.0[i] |= rhs.0[i];
        }
    }
}

impl BitOr<&USizeSet> for &USizeSet {
    type Output = USizeSet;
    fn bitor(self, rhs: &USizeSet) -> Self::Output {
        let mut result = rhs.clone();
        result |= self;
        result
    }
}

impl ToTokens for USizeSet {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut vec_inner = TokenStream::new();
        vec_inner.append_separated(self.0.iter(), Punct::new(',', Spacing::Alone));
        tokens.append_all(quote! { vec![#vec_inner] });
    }
}

pub struct USizeSetIterator<'a> {
    ind: usize,
    cur: u64,
    set: &'a USizeSet,
}

impl<'a> Iterator for USizeSetIterator<'a> {
    type Item = usize;
    fn next(&mut self) -> Option<Self::Item> {
        while self.cur == 0 && self.ind > 0 {
            self.ind -= 1;
            self.cur = self.set.0[self.ind];
        }
        if self.cur == 0 {
            return None;
        }
        let lsb = self.cur.trailing_zeros() as usize;
        self.cur ^= 1 << lsb;
        Some(lsb + self.ind * 64)
    }
}

pub struct IndexableMap<T, U>
where
    T: Clone + Eq + Ord + PartialEq + PartialOrd,
{
    map: BTreeMap<Rc<T>, usize>,
    vec: Vec<(Rc<T>, U)>,
}

impl<T, U> Debug for IndexableMap<T, U>
where
    T: Debug + Clone + Eq + Ord + PartialEq + PartialOrd,
    U: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("{\n")?;
        for (i, (t, u)) in self.vec.iter().enumerate() {
            write!(f, "    {i} @ {:?}: {:?},\n", t, u)?
        }
        f.write_str("}")?;
        Ok(())
    }
}

impl<T, U> IndexableMap<T, U>
where
    T: Clone + Eq + Ord + PartialEq + PartialOrd,
{
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn last(&self) -> Option<(Rc<T>, &U)> {
        self.vec.last().map(|(t, u)| (Rc::clone(t), u))
    }

    pub fn get_ind(&self, x: &T) -> Option<usize> {
        self.map.get(x).map(|x| *x)
    }

    pub fn iter(&self) -> impl Iterator<Item = &(Rc<T>, U)> {
        self.vec.iter()
    }

    /// @return (inserted elements index, true if the element is new)
    pub fn push(&mut self, t: T, u: U) -> (usize, bool) {
        if let Some(ind) = self.map.get(&t) {
            (*ind, false)
        } else {
            let rc = Rc::new(t);
            self.map.insert(Rc::clone(&rc), self.len());
            self.vec.push((rc, u));
            (self.vec.len() - 1, true)
        }
    }

    pub fn to_vec(self) -> Vec<(T, U)> {
        let IndexableMap { vec, .. } = self;
        vec.into_iter()
            .map(|(t, u)| (Rc::try_unwrap(t).ok().unwrap(), u))
            .collect()
    }
}

impl<T, U> Index<usize> for IndexableMap<T, U>
where
    T: Clone + Eq + Ord + PartialEq + PartialOrd,
{
    fn index(&self, index: usize) -> &Self::Output {
        &self.vec[index]
    }
    type Output = (Rc<T>, U);
}

impl<T, U> IndexMut<usize> for IndexableMap<T, U>
where
    T: Clone + Eq + Ord + PartialEq + PartialOrd,
{
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.vec[index]
    }
}

impl<T, U> From<Vec<(T, U)>> for IndexableMap<T, U>
where
    T: Clone + Eq + Ord + PartialEq + PartialOrd,
{
    fn from(items: Vec<(T, U)>) -> Self {
        let vec: Vec<_> = items.into_iter().map(|(t, u)| (Rc::new(t), u)).collect();
        Self {
            map: BTreeMap::from_iter(vec.iter().enumerate().map(|(i, (t, _))| (Rc::clone(t), i))),
            vec,
        }
    }
}

impl<T, U, const N: usize> From<[(T, U); N]> for IndexableMap<T, U>
where
    T: Clone + Eq + Ord + PartialEq + PartialOrd,
{
    fn from(items: [(T, U); N]) -> Self {
        let vec: Vec<_> = items.into_iter().map(|(t, u)| (Rc::new(t), u)).collect();
        Self {
            map: BTreeMap::from_iter(vec.iter().enumerate().map(|(i, (t, _))| (Rc::clone(t), i))),
            vec,
        }
    }
}

impl<T, U> ToTokens for IndexableMap<T, U>
where
    T: ToTokens + Clone + Eq + Ord + PartialEq + PartialOrd,
    U: ToTokens,
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut arr_inner = TokenStream::new();
        arr_inner.append_separated(
            self.vec.iter().map(|(t, u)| quote! { (#t, #u) }),
            Punct::new(',', Spacing::Alone),
        );
        tokens.append_all(quote! { vec![#arr_inner] });
    }
}
