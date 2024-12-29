use std::{fmt::Display, iter::FromIterator};

use super::{
    AncestorNodeIter, ChildIndexIter, ChildNodeIter, Louds, LoudsError, LoudsIndex, LoudsNodeNum,
};
use fid::FID;

fn fid_from_str<T: FID + FromIterator<bool>>(s: &str) -> T {
    let s = s.bytes().filter_map(|c| match c {
        b'0' => Some(false),
        b'1' => Some(true),
        b'_' => None,
        b => panic!("byte '{}' not allowed", b),
    });
    T::from_iter(s)
}

impl Display for LoudsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (ones, zeros) = (self.ones, self.zeros);
        let index = zeros + ones;
        let sign = if zeros <= ones + 1 { "!=" } else { ">" };
        write!(
            f,
            "at {}: rank0 ({}) {} rank1 ({})) + 1",
            index, zeros, sign, ones
        )
    }
}

impl<T: FID + FromIterator<bool>> TryFrom<&str> for Louds<T>
where
    for<'i> &'i T: IntoIterator<Item = bool>,
{
    type Error = LoudsError;

    /// Builds [`Louds`] from LBS (LOUDS Bit vector).
    fn try_from(s: &str) -> Result<Self, LoudsError> {
        let lbs = fid_from_str(s);
        Louds::new(lbs)
    }
}

impl<T: FID> TryFrom<&[bool]> for Louds<T>
where
    for<'i> &'i T: IntoIterator<Item = bool>,
    for<'s> T: From<&'s [bool]>,
{
    type Error = LoudsError;

    /// Builds [`Louds`] from LBS (LOUDS Bit vector).
    fn try_from(bits: &[bool]) -> Result<Self, LoudsError> {
        let lbs = T::from(bits);
        Louds::new(lbs)
    }
}

impl<T: FID> Louds<T> {
    /// Create a new [`Louds`] from a [`FID`].
    pub fn new(lbs: T) -> Result<Self, LoudsError>
    where
        for<'i> &'i T: IntoIterator<Item = bool>,
    {
        Self::validate_lbs(&lbs)?;
        Ok(Louds { lbs })
    }

    /// Create a new [`Louds`] from a [`FID`] without validation.
    pub unsafe fn new_unchecked(lbs: T) -> Self {
        Louds { lbs }
    }

    /// # Panics
    /// `node_num` does not exist in this LOUDS.
    pub fn node_num_to_index(&self, node_num: LoudsNodeNum) -> LoudsIndex {
        assert!(node_num.0 > 0);

        let index = self
            .lbs
            .min_select(true, node_num.0)
            .unwrap_or_else(|| panic!("NodeNum({}) does not exist in this LOUDS", node_num.0,));
        LoudsIndex(index)
    }

    /// # Panics
    /// `index` does not point to any node in this LOUDS.
    pub fn index_to_node_num(&self, index: LoudsIndex) -> LoudsNodeNum {
        self.validate_index(index);

        let node_num = self.lbs.rank1(index.0 + 1);
        LoudsNodeNum(node_num)
    }

    /// # Panics
    /// - `index` does not point to any node in this LOUDS.
    /// - `index == 0`: (node#1 is root and doesn't have parent)
    pub fn child_to_parent(&self, index: LoudsIndex) -> LoudsNodeNum {
        self.validate_index(index);
        assert!(index.0 != 0, "node#1 is root and doesn't have parent");

        let parent_node_num = self.lbs.rank0(index.0 + 1);
        LoudsNodeNum(parent_node_num)
    }

    /// Return an iterator to the `child` and its ancestors' node numbers.
    pub fn child_to_ancestors(&self, child: LoudsNodeNum) -> AncestorNodeIter<'_, T> {
        AncestorNodeIter {
            inner: self,
            node: child,
        }
    }

    /// # Panics
    /// `node_num` does not exist in this LOUDS.
    pub fn parent_to_children(&self, node_num: LoudsNodeNum) -> Vec<LoudsIndex> {
        self.parent_to_children_indices(node_num).collect()
    }

    /// # Panics
    /// `node_num` does not exist in this LOUDS.
    pub fn parent_to_children_indices(&self, node_num: LoudsNodeNum) -> ChildIndexIter<'_, T> {
        assert!(node_num.0 > 0);

        ChildIndexIter {
            inner: self,
            node: node_num,
            start: None,
            end: None,
        }
    }

    /// # Panics
    /// `node_num` does not exist in this LOUDS.
    pub fn parent_to_children_nodes(&self, node_num: LoudsNodeNum) -> ChildNodeIter<'_, T> {
        ChildNodeIter(self.parent_to_children_indices(node_num))
    }

    /// Checks if `lbs` satisfies conditions in `O(N)` where `N` is length:
    /// - Starts from "10"
    /// - In the range of `[0..i]` for any `i < N`;
    ///     - `rank0 <= rank1 + 1`, because each node:
    ///         - has one '0' (including virtual root; node num = 0) .
    ///         - is derived from one '1'.
    /// - In the range of `[0, N)`;
    ///     - `rank0 == rank1 + 1`
    fn validate_lbs<'a>(lbs: &'a T) -> Result<(), LoudsError>
    where
        for<'i> &'i T: IntoIterator<Item = bool>,
    {
        let (mut cnt0, mut cnt1) = (0u64, 0u64);
        macro_rules! err {
            () => {
                LoudsError {
                    ones: cnt1,
                    zeros: cnt0,
                }
            };
        }

        let mut iter = lbs.into_iter();

        let b0 = iter.next().ok_or(err!())?;
        if b0 {
            cnt1 += 1;
        } else {
            cnt0 += 1;
            return Err(err!());
        }

        let b1 = iter.next().ok_or(err!())?;
        if b1 {
            cnt1 += 1;
            return Err(err!());
        } else {
            cnt0 += 1;
        }

        for bit in iter {
            if bit {
                cnt1 += 1
            } else {
                cnt0 += 1
            };

            if !(cnt0 <= cnt1 + 1) {
                return Err(err!());
            }
        }
        if cnt0 != cnt1 + 1 {
            return Err(err!());
        }
        Ok(())
    }

    /// # Panics
    /// `index` does not point to any node in this LOUDS.
    fn validate_index(&self, index: LoudsIndex) {
        assert!(self.lbs.get(index.0), "LBS[index={:?}] must be '1'", index,);
    }
}

impl<'a, T: FID> ChildIndexIter<'a, T> {
    /// Return the length of the iterator.
    ///
    /// It costs _O(log N)_ if the iterator has not had `.next()` and
    /// `.next_back()` called.
    ///
    /// Question: Why not implement [std::iter::ExactSizeIterator]? One could
    /// but they'd be required to do it one of two ways because its signature is
    /// `fn len(&self) -> usize`; `&self` is not mutable:
    ///
    /// 1. Use interior mutability in [ChildIndexIter]. This was attempted with
    /// a [std::cell::RefCell] but it hurt performance slightly.
    ///
    /// 2. Initialize [ChildIndexIter] with the start and end. However
    ///    initializing start and end costs _O(log N)_ each.
    pub fn len(&mut self) -> usize {
        if self.start.is_none() {
            self.start = Some(
                self.inner
                    .lbs
                    .min_select(false, self.node.0)
                    .unwrap_or_else(|| {
                        panic!("NodeNum({}) does not exist in this LOUDS", self.node.0,)
                    })
                    + 1,
            );
        }
        if self.end.is_none() {
            self.end = Some(
                self.inner
                    .lbs
                    .min_select(false, self.node.0 + 1)
                    .unwrap_or_else(|| {
                        panic!("NodeNum({}) does not exist in this LOUDS", self.node.0 + 1,)
                    })
                    - 1,
            );
        }
        let start = self.start.unwrap();
        let end = self.end.unwrap();
        (end + 1 - start) as usize
    }

    /// Returns whether the iterator is empty.
    pub fn is_empty(&mut self) -> bool {
        self.len() == 0
    }
}

impl<'a, T: FID> Iterator for ChildIndexIter<'a, T> {
    type Item = LoudsIndex;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.start.is_none() {
            self.start = Some(
                self.inner
                    .lbs
                    .min_select(false, self.node.0)
                    .unwrap_or_else(|| {
                        panic!("NodeNum({}) does not exist in this LOUDS", self.node.0,)
                    })
                    + 1,
            );
        }
        let start = self.start.unwrap();
        self.end
            .map(|last| start <= last)
            .unwrap_or_else(|| self.inner.lbs.get(start))
            .then(|| {
                self.start = Some(start + 1);
                LoudsIndex(start)
            })
    }
}

impl<'a, T: FID> DoubleEndedIterator for ChildIndexIter<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.end.is_none() {
            self.end = Some(
                self.inner
                    .lbs
                    .min_select(false, self.node.0 + 1)
                    .unwrap_or_else(|| {
                        panic!("NodeNum({}) does not exist in this LOUDS", self.node.0 + 1,)
                    })
                    - 1,
            );
        }
        let end = self.end.unwrap();
        self.start
            .map(|first| first <= end)
            .unwrap_or_else(|| self.inner.lbs.get(end))
            .then(|| {
                self.end = Some(end - 1);
                LoudsIndex(end)
            })
    }
}

impl<'a, T: FID> Iterator for ChildNodeIter<'a, T> {
    type Item = LoudsNodeNum;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .map(|index| self.0.inner.index_to_node_num(index))
    }
}

impl<'a, T: FID> Iterator for AncestorNodeIter<'a, T> {
    type Item = LoudsNodeNum;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.node.0 <= 1 {
            None
        } else {
            let result = self.node;
            let index = self.inner.node_num_to_index(self.node);
            self.node = LoudsNodeNum(self.inner.lbs.rank0(index.0 + 1));
            Some(result)
        }
    }
}

impl<'a, T: FID> DoubleEndedIterator for ChildNodeIter<'a, T> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0
            .next_back()
            .map(|index| self.0.inner.index_to_node_num(index))
    }
}

impl<'a, T: FID> ChildNodeIter<'a, T> {
    /// See [ChildIndexIter::len].
    pub fn len(&mut self) -> usize {
        self.0.len()
    }

    /// Returns whether the iterator is empty.
    pub fn is_empty(&mut self) -> bool {
        self.len() == 0
    }
}

#[cfg(test)]
mod validate_lbs_success_tests {
    use crate::Louds;
    use fid::BitVector;

    macro_rules! parameterized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let s = $value;
                let fid = super::fid_from_str::<BitVector>(s);
                Louds::validate_lbs(&fid).unwrap();
            }
        )*
        }
    }

    parameterized_tests! {
        t1: "10_0",
        t2: "10_10_0",
        t3: "10_1110_10_0_1110_0_0_10_110_0_0_0",
        t4: "10_11111111110_0_0_0_0_0_0_0_0_0_0",
    }
}

#[cfg(test)]
mod validate_lbs_failure_tests {
    use crate::Louds;
    use fid::BitVector;

    macro_rules! parameterized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            #[should_panic]
            fn $name() {
                let s = $value;
                let fid = super::fid_from_str::<BitVector>(s);
                Louds::validate_lbs(&fid).unwrap();
            }
        )*
        }
    }

    parameterized_tests! {
        t1: "0",
        t2: "1",
        t3: "00",
        t4: "01",
        t5: "10",
        t6: "11",
        t7: "00_0",
        t8: "01_0",
        t9: "11_0",
        t10: "10_1",
        t11: "10_10",
        t12: "10_01",
        t13: "10_1110_10_0_1110_0_0_10_110_0_0_1",
    }
}

#[cfg(test)]
mod node_num_to_index_success_tests {
    use crate::{BitLouds, LoudsIndex, LoudsNodeNum};

    macro_rules! parameterized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (in_s, node_num, expected_index) = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let index = louds.node_num_to_index(LoudsNodeNum(node_num));
                assert_eq!(index, LoudsIndex(expected_index));
            }
        )*
        }
    }

    parameterized_tests! {
        t1_1: ("10_0", 1, 0),

        t2_1: ("10_10_0", 1, 0),
        t2_2: ("10_10_0", 2, 2),

        t3_1: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 1, 0),
        t3_2: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 2, 2),
        t3_3: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 3, 3),
        t3_4: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 4, 4),
        t3_5: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 5, 6),
        t3_6: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 6, 9),
        t3_7: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 7, 10),
        t3_8: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 8, 11),
        t3_9: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 9, 15),
        t3_10: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 10, 17),
        t3_11: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 11, 18),
    }
}

#[cfg(test)]
mod node_num_to_index_failure_tests {
    use crate::{BitLouds, LoudsNodeNum};

    macro_rules! parameterized_node_not_found_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            #[should_panic]
            fn $name() {
                let (in_s, node_num) = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let _ = louds.node_num_to_index(LoudsNodeNum(node_num));
            }
        )*
        }
    }

    parameterized_node_not_found_tests! {
        t1_1: ("10_0", 0),
        t1_2: ("10_0", 2),

        t2_1: ("10_10_0", 0),
        t2_2: ("10_10_0", 3),

        t3_1: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 0),
        t3_2: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 12),
    }
}

#[cfg(test)]
mod index_to_node_num_success_tests {
    use crate::{BitLouds, LoudsIndex, LoudsNodeNum};

    macro_rules! parameterized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (in_s, index, expected_node_num) = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let node_num = louds.index_to_node_num(LoudsIndex(index));
                assert_eq!(node_num, LoudsNodeNum(expected_node_num));
            }
        )*
        }
    }

    parameterized_tests! {
        t1_1: ("10_0", 0, 1),

        t2_1: ("10_10_0", 0, 1),
        t2_2: ("10_10_0", 2, 2),

        t3_1: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 0, 1),
        t3_2: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 2, 2),
        t3_3: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 3, 3),
        t3_4: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 4, 4),
        t3_5: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 6, 5),
        t3_6: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 9, 6),
        t3_7: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 10, 7),
        t3_8: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 11, 8),
        t3_9: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 15, 9),
        t3_10: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 17, 10),
        t3_11: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 18, 11),
    }
}

#[cfg(test)]
mod index_to_node_num_failure_tests {
    use crate::{BitLouds, LoudsIndex};

    macro_rules! parameterized_index_not_point_to_node_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            #[should_panic]
            fn $name() {
                let (in_s, index) = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let _ = louds.index_to_node_num(LoudsIndex(index));
            }
        )*
        }
    }

    parameterized_index_not_point_to_node_tests! {
        t1_1: ("10_0", 1),
        t1_2: ("10_0", 3),

        t2_1: ("10_10_0", 1),
        t2_2: ("10_10_0", 3),
        t2_3: ("10_10_0", 4),
        t2_4: ("10_10_0", 5),

        t3_1: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 1),
        t3_2: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 5),
        t3_3: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 7),
        t3_4: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 8),
        t3_5: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 12),
        t3_6: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 13),
        t3_7: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 14),
        t3_8: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 16),
        t3_9: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 19),
        t3_10: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 20),
        t3_11: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 21),
        t3_12: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 22),
        t3_13: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 23),
        t3_14: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 24),
    }
}

#[cfg(test)]
mod child_to_parent_success_tests {
    use crate::{BitLouds, LoudsIndex, LoudsNodeNum};

    macro_rules! parameterized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (in_s, index, expected_parent) = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let parent = louds.child_to_parent(LoudsIndex(index));
                assert_eq!(parent, LoudsNodeNum(expected_parent));
            }
        )*
        }
    }

    parameterized_tests! {
        t2_1: ("10_10_0", 2, 1),

        t3_1: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 2, 1),
        t3_2: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 3, 1),
        t3_3: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 4, 1),
        t3_4: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 6, 2),
        t3_5: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 9, 4),
        t3_6: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 10, 4),
        t3_7: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 11, 4),
        t3_8: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 15, 7),
        t3_9: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 17, 8),
        t3_10: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 18, 8),
    }
}

#[cfg(test)]
mod child_to_parent_failure_tests {
    use crate::{BitLouds, LoudsIndex};

    macro_rules! parameterized_index_not_point_to_node_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            #[should_panic]
            fn $name() {
                let (in_s, index) = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let _ = louds.child_to_parent(LoudsIndex(index));
            }
        )*
        }
    }

    parameterized_index_not_point_to_node_tests! {
        t1_1: ("10_0", 1),
        t1_2: ("10_0", 3),

        t2_1: ("10_10_0", 1),
        t2_2: ("10_10_0", 3),
        t2_3: ("10_10_0", 4),
        t2_4: ("10_10_0", 5),

        t3_1: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 1),
        t3_2: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 5),
        t3_3: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 7),
        t3_4: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 8),
        t3_5: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 12),
        t3_6: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 13),
        t3_7: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 14),
        t3_8: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 16),
        t3_9: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 19),
        t3_10: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 20),
        t3_11: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 21),
        t3_12: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 22),
        t3_13: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 23),
        t3_14: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 24),
    }

    macro_rules! parameterized_root_not_have_parent_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            #[should_panic]
            fn $name() {
                let in_s = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let _ = louds.child_to_parent(LoudsIndex(0));
            }
        )*
        }
    }

    parameterized_root_not_have_parent_tests! {
        t1: "10_0",
        t2: "10_10_0",
        t3: "10_1110_10_0_1110_0_0_10_110_0_0_0",
    }
}

#[cfg(test)]
mod parent_to_children_success_tests {
    use crate::{BitLouds, LoudsIndex, LoudsNodeNum};

    macro_rules! parameterized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (in_s, node_num, expected_children) = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let children: Vec<_> = louds.parent_to_children(LoudsNodeNum(node_num));
                assert_eq!(children, expected_children.iter().map(|c| LoudsIndex(*c)).collect::<Vec<LoudsIndex>>());
            }
        )*
        }
    }

    parameterized_tests! {
        t1_1: ("10_0", 1, vec!()),

        t2_1: ("10_10_0", 1, vec!(2)),
        t2_2: ("10_10_0", 2, vec!()),

        t3_1: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 1, vec!(2, 3, 4)),
        t3_2: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 2, vec!(6)),
        t3_3: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 3, vec!()),
        t3_4: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 4, vec!(9, 10, 11)),
        t3_5: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 5, vec!()),
        t3_6: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 6, vec!()),
        t3_7: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 7, vec!(15)),
        t3_8: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 8, vec!(17, 18)),
        t3_9: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 9, vec!()),
        t3_10: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 10, vec!()),
        t3_11: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 11, vec!()),
    }
}

#[cfg(test)]
mod child_to_ancestors_success_tests {
    use crate::{BitLouds, LoudsNodeNum};

    macro_rules! parameterized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (in_s, node_num, expected_children) = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let children: Vec<_> = louds.child_to_ancestors(LoudsNodeNum(node_num)).collect();
                assert_eq!(children, expected_children.iter().map(|c| LoudsNodeNum(*c)).collect::<Vec<LoudsNodeNum>>());
            }
        )*
        }
    }

    parameterized_tests! {
        t1_1: ("10_0", 1, vec!()),

        t2_1: ("10_10_0", 1, vec!()),
        t2_2: ("10_10_0", 2, vec!(2)),

        t3_1: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 1, vec!()),
        t3_2: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 2, vec!(2)),
        t3_3: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 3, vec!(3)),
        t3_4: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 4, vec!(4)),
        t3_5: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 5, vec!(5, 2)),
        t3_6: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 6, vec!(6, 4)),
        t3_7: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 7, vec!(7, 4)),
        t3_8: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 8, vec!(8, 4)),
        t3_9: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 9, vec!(9, 7, 4)),
        t3_10: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 10, vec!(10, 8, 4)),
        t3_11: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 11, vec!(11, 8, 4)),
    }
}

#[cfg(test)]
mod child_to_ancestors_failure_tests {
    use crate::{BitLouds, LoudsIndex};

    macro_rules! parameterized_index_not_point_to_node_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            #[should_panic]
            fn $name() {
                let (in_s, index) = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let node = louds.index_to_node_num(LoudsIndex(index));
                let _ = louds.child_to_ancestors(node);
            }
        )*
        }
    }

    parameterized_index_not_point_to_node_tests! {
        t1_1: ("10_0", 1),
        t1_2: ("10_0", 3),

        t2_1: ("10_10_0", 1),
        t2_2: ("10_10_0", 3),
        t2_3: ("10_10_0", 4),
        t2_4: ("10_10_0", 5),

        t3_1: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 1),
        t3_2: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 5),
        t3_3: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 7),
        t3_4: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 8),
        t3_5: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 12),
        t3_6: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 13),
        t3_7: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 14),
        t3_8: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 16),
        t3_9: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 19),
        t3_10: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 20),
        t3_11: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 21),
        t3_12: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 22),
        t3_13: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 23),
        t3_14: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 24),
    }

    macro_rules! parameterized_root_not_have_parent_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            #[should_panic]
            fn $name() {
                let in_s = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let _ = louds.child_to_parent(LoudsIndex(0));
            }
        )*
        }
    }

    parameterized_root_not_have_parent_tests! {
        t1: "10_0",
        t2: "10_10_0",
        t3: "10_1110_10_0_1110_0_0_10_110_0_0_0",
    }
}

#[cfg(test)]
mod parent_to_children_indices_success_tests {
    use crate::{BitLouds, LoudsIndex, LoudsNodeNum};

    macro_rules! parameterized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (in_s, node_num, expected_children) = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let children: Vec<_> = louds.parent_to_children_indices(LoudsNodeNum(node_num)).collect();
                assert_eq!(children, expected_children.iter().map(|c| LoudsIndex(*c)).collect::<Vec<LoudsIndex>>());
            }
        )*
        }
    }

    parameterized_tests! {
        t1_1: ("10_0", 1, vec!()),

        t2_1: ("10_10_0", 1, vec!(2)),
        t2_2: ("10_10_0", 2, vec!()),

        t3_1: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 1, vec!(2, 3, 4)),
        t3_2: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 2, vec!(6)),
        t3_3: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 3, vec!()),
        t3_4: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 4, vec!(9, 10, 11)),
        t3_5: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 5, vec!()),
        t3_6: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 6, vec!()),
        t3_7: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 7, vec!(15)),
        t3_8: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 8, vec!(17, 18)),
        t3_9: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 9, vec!()),
        t3_10: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 10, vec!()),
        t3_11: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 11, vec!()),
    }
}

#[cfg(test)]
mod parent_to_children_indices_rev_success_tests {
    use crate::{BitLouds, LoudsIndex, LoudsNodeNum};

    macro_rules! parameterized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (in_s, node_num, expected_children) = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let children: Vec<_> = louds.parent_to_children_indices(LoudsNodeNum(node_num)).rev().collect();
                assert_eq!(children, expected_children.iter().map(|c| LoudsIndex(*c)).collect::<Vec<LoudsIndex>>());
            }
        )*
        }
    }

    parameterized_tests! {
        t1_1: ("10_0", 1, vec!()),

        t2_1: ("10_10_0", 1, vec!(2)),
        t2_2: ("10_10_0", 2, vec!()),

        t3_1: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 1, vec!(4, 3, 2)),
        t3_2: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 2, vec!(6)),
        t3_3: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 3, vec!()),
        t3_4: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 4, vec!(11, 10, 9)),
        t3_5: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 5, vec!()),
        t3_6: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 6, vec!()),
        t3_7: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 7, vec!(15)),
        t3_8: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 8, vec!(18, 17)),
        t3_9: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 9, vec!()),
        t3_10: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 10, vec!()),
        t3_11: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 11, vec!()),
    }
}

#[cfg(test)]
mod parent_to_children_indices_len_success_tests {
    use crate::{BitLouds, LoudsNodeNum};

    macro_rules! parameterized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (in_s, node_num, expected_size) = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let mut iter = louds.parent_to_children_indices(LoudsNodeNum(node_num));
                assert_eq!(iter.len(), expected_size);
            }
        )*
        }
    }

    parameterized_tests! {
        t1_1: ("10_0", 1, 0),

        t2_1: ("10_10_0", 1, 1),
        t2_2: ("10_10_0", 2, 0),

        t3_1: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 1, 3),
        t3_2: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 2, 1),
        t3_3: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 3, 0),
        t3_4: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 4, 3),
        t3_5: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 5, 0),
        t3_6: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 6, 0),
        t3_7: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 7, 1),
        t3_8: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 8, 2),
        t3_9: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 9, 0),
        t3_10: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 10, 0),
        t3_11: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 11, 0),
    }
}

#[cfg(test)]
mod parent_to_children_indices_next_back_success_tests {
    use crate::{BitLouds, LoudsIndex, LoudsNodeNum};

    macro_rules! parameterized_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (in_s, node_num, expected_children) = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let mut front = Vec::new();
                let mut back = Vec::new();
                let mut iter = louds.parent_to_children_indices(LoudsNodeNum(node_num));
                while let Some(x) = iter.next() {
                    front.push(x);
                    if let Some(y) = iter.next_back() {
                        back.push(y);
                    }
                }
                front.extend(back.into_iter().rev());
                assert_eq!(front, expected_children.iter().map(|c| LoudsIndex(*c)).collect::<Vec<LoudsIndex>>());
            }
        )*
        }
    }

    parameterized_tests! {
        t1_1: ("10_0", 1, vec!()),

        t2_1: ("10_10_0", 1, vec!(2)),
        t2_2: ("10_10_0", 2, vec!()),

        t3_1: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 1, vec!(2, 3, 4)),
        t3_2: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 2, vec!(6)),
        t3_3: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 3, vec!()),
        t3_4: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 4, vec!(9, 10, 11)),
        t3_5: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 5, vec!()),
        t3_6: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 6, vec!()),
        t3_7: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 7, vec!(15)),
        t3_8: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 8, vec!(17, 18)),
        t3_9: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 9, vec!()),
        t3_10: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 10, vec!()),
        t3_11: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 11, vec!()),
    }
}

#[cfg(test)]
mod parent_to_children_failure_tests {
    use crate::{BitLouds, LoudsNodeNum};

    macro_rules! parameterized_node_not_found_tests {
        ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            #[should_panic]
            fn $name() {
                let (in_s, node_num) = $value;
                let louds = BitLouds::try_from(in_s).unwrap();
                let _: Vec<_> = louds.parent_to_children(LoudsNodeNum(node_num));
            }
        )*
        }
    }

    parameterized_node_not_found_tests! {
        t1_1: ("10_0", 0),
        t1_2: ("10_0", 2),

        t2_1: ("10_10_0", 0),
        t2_2: ("10_10_0", 3),

        t3_1: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 0),
        t3_2: ("10_1110_10_0_1110_0_0_10_110_0_0_0", 12),
    }
}
