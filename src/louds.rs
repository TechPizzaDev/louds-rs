mod louds_impl;

use fid::{BitVector, FID};

pub type BitLouds = Louds<BitVector>;

/// LOUDS (Level-Order Unary Degree Sequence).
///
/// This class can handle tree structure of virtually **arbitrary number of nodes**.
///
/// In fact, _N_ (number of nodes in the tree) is designed to be limited to: _N < 2^64 / 2_, while each node is represented in 2bits in average.<br>
/// It should be enough for almost all usecases since a binary data of length of _2^63_ consumes _2^20 = 1,048,576_ TB (terabytes), which is hard to handle by state-of-the-art computer architecture.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "mem_dbg", derive(mem_dbg::MemDbg, mem_dbg::MemSize))]
pub struct Louds<T: FID = BitVector> {
    lbs: T,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoudsError {
    ones: u64,
    zeros: u64,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "mem_dbg", derive(mem_dbg::MemDbg, mem_dbg::MemSize))]
#[repr(transparent)]
/// Node number of [Louds](struct.Louds.html) tree
pub struct LoudsNodeNum(pub u64);

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "mem_dbg", derive(mem_dbg::MemDbg, mem_dbg::MemSize))]
#[repr(transparent)]
/// Index of [Louds](struct.Louds.html) tree
pub struct LoudsIndex(pub u64);

/// An index iterator
pub struct ChildIndexIter<'a, T: FID> {
    inner: &'a Louds<T>,
    node: LoudsNodeNum,
    start: Option<u64>,
    end: Option<u64>,
}
/// A node iterator
pub struct ChildNodeIter<'a, T: FID>(ChildIndexIter<'a, T>);

/// An ancestor node iterator
pub struct AncestorNodeIter<'a, T: FID> {
    inner: &'a Louds<T>,
    node: LoudsNodeNum,
}
