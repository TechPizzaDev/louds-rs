use criterion::{criterion_group, criterion_main, Criterion};

fn c() -> Criterion {
    Criterion::default().with_plots()
}

mod louds {
    use criterion::{BenchmarkId, Criterion};
    use louds::{BitLouds, LoudsIndex, LoudsNodeNum};

    const SIZES: [u64; 5] = [1 << 11, 1 << 12, 1 << 13, 1 << 14, 1 << 15];

    fn generate_binary_tree_lbs_bits(n_nodes: u64) -> Vec<bool> {
        assert!(
            SIZES.iter().any(|n| n - 1 == n_nodes),
            "Only 2^m - 1 nodes (complete binary tree) is supported"
        );

        let mut bits = vec![true, false];

        // Nodes
        for _ in 1..=(n_nodes / 2) {
            bits.append(&mut vec![true, true, false]);
        }

        // Leaves
        for _ in (n_nodes / 2 + 1)..=(n_nodes) {
            bits.push(false);
        }

        bits
    }

    fn generate_binary_tree_lbs_string(n_nodes: u64) -> String {
        generate_binary_tree_lbs_bits(n_nodes)
            .iter()
            .map(|bit| if *bit { '1' } else { '0' })
            .collect()
    }

    pub fn from_bits_benchmark(c: &mut Criterion) {
        for n in SIZES {
            c.bench_with_input(
                BenchmarkId::new("Louds::from::<&[bool]>(&[...(bin tree of N nodes)])", n),
                &generate_binary_tree_lbs_bits(n - 1),
                |b, v| b.iter(|| BitLouds::from(v.as_slice())),
            );
        }
    }

    pub fn from_str_benchmark(c: &mut Criterion) {
        for n in SIZES {
            c.bench_with_input(
                BenchmarkId::new("Louds::from::<&str>(\"...(bin tree of N nodes)\")", n),
                &generate_binary_tree_lbs_string(n - 1),
                |b, s| b.iter(|| BitLouds::from(s.as_str())),
            );
        }
    }

    pub fn node_num_to_index_benchmark(c: &mut Criterion) {
        for n in SIZES {
            c.bench_with_input(
                BenchmarkId::new("Louds(N)::node_num_to_index()", n),
                &BitLouds::from(generate_binary_tree_lbs_bits(n - 1).as_slice()),
                |b, l| b.iter(|| l.node_num_to_index(LoudsNodeNum(n - 1))),
            );
        }
    }

    pub fn index_to_node_num_benchmark(c: &mut Criterion) {
        for n in SIZES {
            c.bench_with_input(
                BenchmarkId::new("Louds(N)::index_to_node_num()", n),
                &BitLouds::from(generate_binary_tree_lbs_bits(n - 1).as_slice()),
                |b, l| b.iter(|| l.index_to_node_num(LoudsIndex(n / 2 + 1))),
            );
        }
    }

    pub fn parent_to_children_benchmark(c: &mut Criterion) {
        for n in SIZES {
            c.bench_with_input(
                BenchmarkId::new("Louds(N)::parent_to_children()", n),
                &BitLouds::from(generate_binary_tree_lbs_bits(n - 1).as_slice()),
                |b, l| b.iter(|| l.parent_to_children(LoudsNodeNum(n - 1))),
            );
        }
    }

    pub fn child_to_parent_benchmark(c: &mut Criterion) {
        for n in SIZES {
            c.bench_with_input(
                BenchmarkId::new("Louds(N)::child_to_parent()", n),
                &BitLouds::from(generate_binary_tree_lbs_bits(n - 1).as_slice()),
                |b, l| b.iter(|| l.child_to_parent(LoudsIndex(n / 2 + 1))),
            );
        }
    }
}

criterion_group!(
    name = benches;
    config = c();
    targets =
        louds::from_bits_benchmark,
        louds::from_str_benchmark,
        louds::node_num_to_index_benchmark,
        louds::index_to_node_num_benchmark,
        louds::parent_to_children_benchmark,
        louds::child_to_parent_benchmark,
);
criterion_main!(benches);
