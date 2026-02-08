# Sorting Algorithms - Ephapax Implementation

Comprehensive sorting algorithm implementations demonstrating Ephapax's linear type system.

## Status

**Specification Only** - Waiting for Ephapax compiler completion.

## Algorithms Implemented

| Algorithm | Time Complexity | Space | Linear Types Feature |
|-----------|-----------------|-------|---------------------|
| Bubble Sort | O(n²) | O(1) | In-place mutation |
| Selection Sort | O(n²) | O(1) | In-place swap |
| Insertion Sort | O(n²) | O(1) | In-place shift |
| Merge Sort | O(n log n) | O(n) | Ownership split/merge |
| Quick Sort | O(n log n) avg | O(log n) | Partition by ownership |
| Heap Sort | O(n log n) | O(1) | Borrow for heapify |
| Counting Sort | O(n+k) | O(k) | Region allocation |
| Radix Sort | O(d(n+k)) | O(n+k) | Digit extraction |
| Bucket Sort | O(n+k) avg | O(n) | Float distribution |

## Linear Types in Sorting

### Ownership Transfer
```ephapax
fn merge_sort(arr: [Int]) -> [Int]:  -- Takes ownership
    let (left, right) = arr.split_at(mid)  -- Consumes arr
    ...
```

### In-Place Mutation
```ephapax
fn bubble_sort(arr: [Int]) -> [Int]:
    let mut result = arr  -- Takes ownership
    result.swap(i, j)     -- Mutates owned data
    result               -- Returns ownership
```

### Borrow for Read
```ephapax
fn is_sorted(arr: &[Int]) -> Bool:  -- Borrows, doesn't consume
    for i in 1..arr.len():
        if arr[i-1] > arr[i]: return false
    true
```

## Build (Future)

```bash
ephapax build --target wasm32 sorting.epx
wasmtime sorting.wasm
```

## License

SPDX-License-Identifier: MPL-2.0-or-later
