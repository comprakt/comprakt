This is a safe rust abstraction for the libfirm graph based intermediate
representation and backend for optimising compilers.

---

Files with a `_gen.rs` suffix in this crate are generated. To run
the generator follow the instructions below.

Do the following once to install all dependencies for the generator:

```
cd generator
npm install
```

To run the generator execute:

```
cd generator
npm run generate
```

You only have to run the generator if you have updated its source.  The checked
in `_gen.rs` files should always match the output of the checked out `gen.ts`.
