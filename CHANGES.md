## (trunk)

- Ocaml 5.0 compatible. Thanks to @samoht for the report.
- tweak ordering implementation in `F.of_list`

## v0.3.0 2019-04-09

Semantics cleanup.

Breaking:

- `find` drops `?promote` and never changes the ordering.
- `add` drops `?trim` and never drops bindings.
- `size` -> `weight`
- `items` -> `size`
- `unadd` -> `pop`
- `F.S.fold` and `F.S.iter` iterate in LRU order.
- `F.M.fold` and `F.M.iter` drop `?dir` and always iterate in LRU order.

Other:

- add `F.S.fold_k` and `F.S.iter_k`

To fix client code:

- replace `find ~promote:false` with `find`;
- replace `find` and `find ~promote:true` with `find` and `promote`;
- replace `add ~trim:false` with `add`;
- replace `add` and `add ~trim:true` with `add` and `trim`;
- `s/size/weight/g`, `s/items/size/g`, `s/unadd/pop/g`;
- audit uses of `fold` and `iter` for order-sensitivity.

## v0.2.0 2017-03-31

Breaking changes:

- `resize` no longer drops bindings if the new size pushes the queue over capacity.
- `of_list` has simpler semantics; dropped the `cap` parameter.

Other changes:

- Replace `Lru.M.cache` with more general `Lru.memo`.
- Queues with 0 initial capacity are legal.
- Add `trim` to shrink a queue to its capacity, as queues are no longer guaranteed to
  have size smaller than capacity.
- `find` gets the `promote` parameter, allowing queries that do not change the order.
- `add` gets the `trim` parameter, allowing insertions that do not drop old entries.

## v0.1.1 2016-11-28

* Fix missing dep on `psq` in META.

## v0.1.0 2016-11-22

First release.
