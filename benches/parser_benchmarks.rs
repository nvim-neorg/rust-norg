use std::hint::black_box;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use chumsky::Parser as _;
use rust_norg::{parse, parse_tree, stage_1, stage_2, stage_3, stage_4, NorgASTFlat};

const SMALL: &str = include_str!("inputs/small.norg");
const MEDIUM: &str = include_str!("inputs/medium.norg");
const LARGE: &str = include_str!("inputs/large.norg");

fn parse_tree_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse_tree");
    for (name, input) in [("small", SMALL), ("medium", MEDIUM), ("large", LARGE)] {
        group.throughput(Throughput::Elements(input.lines().count() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), input, |b, input| {
            b.iter(|| parse_tree(black_box(input)))
        });
    }
    group.finish();
}

fn parse_flat_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("parse_flat");
    for (name, input) in [("small", SMALL), ("medium", MEDIUM), ("large", LARGE)] {
        group.throughput(Throughput::Elements(input.lines().count() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), input, |b, input| {
            b.iter(|| parse(black_box(input)))
        });
    }
    group.finish();
}

fn stage_1_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("stage_1");
    for (name, input) in [("small", SMALL), ("medium", MEDIUM), ("large", LARGE)] {
        group.throughput(Throughput::Elements(input.lines().count() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), input, |b, input| {
            b.iter(|| stage_1().parse(black_box(input)))
        });
    }
    group.finish();
}

fn stage_2_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("stage_2");
    for (name, input) in [("small", SMALL), ("medium", MEDIUM), ("large", LARGE)] {
        let tokens = stage_1().parse(input).unwrap();
        group.throughput(Throughput::Elements(tokens.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), &tokens, |b, tokens| {
            b.iter(|| stage_2().parse(black_box(tokens.clone())).unwrap())
        });
    }
    group.finish();
}

fn stage_3_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("stage_3");
    for (name, input) in [("small", SMALL), ("medium", MEDIUM), ("large", LARGE)] {
        let tokens = stage_1().parse(input).unwrap();
        let blocks = stage_2().parse(tokens).unwrap();
        group.throughput(Throughput::Elements(blocks.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), &blocks, |b, blocks| {
            b.iter(|| stage_3().parse(black_box(blocks.clone())).unwrap())
        });
    }
    group.finish();
}

fn stage_4_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("stage_4");
    for (name, input) in [("small", SMALL), ("medium", MEDIUM), ("large", LARGE)] {
        let flat: Vec<NorgASTFlat> = parse(input).unwrap();
        group.throughput(Throughput::Elements(flat.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), &flat, |b, flat| {
            b.iter(|| stage_4(black_box(flat.clone())))
        });
    }
    group.finish();
}

criterion_group!(
    name = benches;
    config = Criterion::default().sample_size(50);
    targets = parse_tree_bench,
        parse_flat_bench,
        stage_1_bench,
        stage_2_bench,
        stage_3_bench,
        stage_4_bench
);
criterion_main!(benches);
