# lab4

```bash
> runhaskell ./Signature.hs keys 32 public.txt private.txt
> runhaskell ./Signature.hs sign private.txt file.txt fileS.txt
> runhaskell ./Signature.hs check public.txt fileS.txt
Signature is valid
> runhaskell ./Signature.hs unsign 32 fileS.txt
```
