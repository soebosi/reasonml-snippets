open Belt;

module List = {
  let all = List.every;
  let any = List.some;
  let bifurcate = (ary, filter: list(bool)) => {
    let (x, y) = List.(zip(ary, filter) |. partition(snd));
    (List.map(x, fst), List.map(y, fst));
  };
  let bifurcateBy = List.partition;
  let compact = ary => List.keep(ary, elm => elm != None);
  let chunk = {
    let rec chunk_ = (ary, i, acc) =>
      switch (List.splitAt(ary, i)) {
      | Some((group, rest)) => chunk_(rest, i, acc @ [group])
      | None when List.length(ary) > 0 => acc @ [ary]
      | _ => acc
      };
    (ary, i) => chunk_(ary, i, []);
  };
  let countBy = (ary, fn, ~id) => {
    let m = Belt.Map.make(~id);
    List.reduce(
      ary,
      m,
      (acc, elm) => {
        let e = fn(elm);
        switch (Belt.Map.get(acc, e)) {
        | Some(count) => Belt.Map.set(acc, e, count + 1)
        | None => Belt.Map.set(acc, e, 1)
        };
      },
    );
  };
  type tree =
    | Leaf
    | Node(int, tree, tree);
  let deepFlatten = t => {
    let rec _deepFlatten = (t, treeList, acc) =>
      switch (t, treeList) {
      | (Leaf, []) => acc
      | (Leaf, [a]) => _deepFlatten(a, [], acc)
      | (Leaf, [hd, ...tl]) => _deepFlatten(hd, tl, acc)
      | (Node(a, t1, t2), _) =>
        _deepFlatten(t1, treeList @ [t2], acc @ [a])
      };
    _deepFlatten(t, [], []);
  };
  let countOccurrences = {
    let rec countOccurrences_ = (arr, value, acc) =>
      switch (arr) {
      | [] => acc
      | [a] when a == value => acc + 1
      | [_] => acc
      | [a, ...b] when a == value => countOccurrences_(b, value, acc + 1)
      | [_, ...b] => countOccurrences_(b, value, acc)
      };
    (arr, value) => countOccurrences_(arr, value, 0);
  };
  let drop = (ary, i) => List.drop(ary, i) |. Option.getWithDefault([]);
  let dropRight = (ary, i) =>
    List.(take(ary, length(ary) - i) |. Option.getWithDefault([]));
  let dropRightWhile = {
    let rec dropRightWhile_ = (ary, func, acc) =>
      switch (ary) {
      | [head, ...tail] when func(head) =>
        dropRightWhile_(tail, func, acc @ [head])
      | _ => acc
      };
    (ary, func) => dropRightWhile_(ary, func, []);
  };
  let everyNth = {
    let rec everyNth_ = (ary, nth, acc, index) => {
      let skip = index mod nth != 0;
      switch (ary) {
      | [] => acc
      | [hd, ...tl] =>
        (skip ? acc : acc @ [hd]) |. everyNth_(tl, nth, _, index + 1)
      };
    };
    (ary, nth) => everyNth_(ary, nth, [], 1);
  };
  let findLast = (ary, fn) =>
    List.(reverse(ary) |. getBy(fn) |. Option.getExn);
  let findLastIndex = (ary, fn) =>
    List.(
      mapWithIndex(ary, (i, elm) => (i, elm))
      |. reverse
      |. getBy(((_i, elm)) => fn(elm))
      |. Option.getExn
      |. fst
    );
  let head = List.headExn;
  let indexOfAll = (ary, elm) =>
    List.(
      mapWithIndex(ary, (i, e) => (i, e))
      |. keep(((_, e)) => e == elm)
      |. map(fst)
    );
  let initial = ary => List.(reverse(ary) |. tailExn |. reverse);
  let range_ = (s, e, step) =>
    Array.rangeBy(s, e - 1, ~step) |. List.fromArray;
  let initialize2DArray = (w, h, val_) =>
    range_(0, w, 1) |. List.map((_) => List.make(h, val_));
  let initializeArrayWithRange = range_;
  let initializeArrayWithRangeRight = (e, s, step) =>
    range_(s, e, step) |. List.reverse;
  let initializeArrayWithValues = List.make;
  let join = (ary, sep) => {
    let [hd, ...tl] = ary;
    List.reduce(tl, hd, (a, b) => a ++ sep ++ b);
  };
  let last = ary => List.(reverse(ary) |. headExn);
  let forEachRight = (ary, fn) => List.(reverse(ary) |. forEach(fn));
  let none = (ary, fn) => ! List.some(ary, fn);
  let takeLast = (ary, i) =>
    List.(reverse(ary) |. take(i) |. Option.getWithDefault([]) |. reverse);
  let takeWhile = dropRightWhile;
  let takeLastWhile = (ary, func) =>
    List.(reverse(ary) |. takeWhile(func) |. reverse);
};
