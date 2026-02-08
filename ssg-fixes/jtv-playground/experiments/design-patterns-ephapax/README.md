# Creational Design Patterns - Ephapax Implementation

Design patterns adapted for linear type systems, demonstrating how ownership semantics affect traditional OOP patterns.

## Status

**Specification Only** - Waiting for Ephapax compiler completion.

## Patterns Implemented

### Singleton
- Linear types ensure exactly one instance
- Global instance via runtime-managed static
- Borrow-only access prevents ownership theft

### Factory
- Factory transfers ownership of created products
- Region-based allocation for products
- `Box<dyn Trait>` for polymorphic returns

### Abstract Factory
- Creates families of related products
- Consistent ownership model across product families
- Each factory method transfers ownership

### Builder
- Method chaining with ownership transfer
- `build()` consumes builder, returns product
- Director encapsulates construction recipes

### Prototype
- Deep clone required for linear types
- No shallow clone (shared ownership not allowed)
- `clone@r()` allocates in specified region

### Object Pool
- Pool maintains ownership of resources
- Borrows issued for use, returned on release
- Prevents resource leaks via linear tracking

## Linear Types Benefits

| Pattern | Benefit |
|---------|---------|
| Singleton | Enforced single instance |
| Factory | Clear ownership transfer |
| Builder | No dangling references |
| Prototype | No accidental sharing |
| Pool | Automatic resource tracking |

## Build (Future)

```bash
ephapax build --target wasm32 creational_patterns.epx
wasmtime creational_patterns.wasm
```

## License

SPDX-License-Identifier: MPL-2.0-or-later
