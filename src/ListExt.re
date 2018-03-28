open Belt;

module List = {
  include Belt.List;
  let all = every;
  let any = some;
  let bifurcate = (ary, filter: list(bool)) => {
    let (x, y) = zip(ary, filter) |. partition(snd);
    (map(x, fst), map(y, fst));
  };
  let bifurcateBy = partition;
  let takeLast = (ary, i) =>
    reverse(ary) |. take(i) |. Option.getWithDefault([]) |. reverse;
  let takeWhile = {
    let rec takeWhile_ = (ary, func, acc) =>
      switch (ary) {
      | [head, ...tail] when func(head) =>
        takeWhile_(tail, func, acc @ [head])
      | _ => acc
      };
    (ary, func) => takeWhile_(ary, func, []);
  };
  let takeLastWhile = (ary, func) =>
    reverse(ary) |. takeWhile(func) |. reverse;
  let chunk = {
    let rec chunk_ = (ary, i, acc) => {
      let len = length(ary);
      if (i <= 0) {
        [];
      } else if (len <= i) {
        acc @ [ary];
      } else {
        chunk_(
          takeLast(ary, len - i),
          i,
          acc @ [take(ary, i) |. Option.getWithDefault([])],
        );
      };
    };
    (ary, i) => chunk_(ary, i, []);
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
  let drop = (ary, i) => takeLast(ary, length(ary) - i);
  let dropRight = (ary, i) =>
    take(ary, length(ary) - i) |. Option.getWithDefault([]);
  let dropRightWhile = takeWhile;
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
  let getExn = a =>
    switch (a) {
    | Some(v) => v
    | None => raise(Not_found)
    };
  let findLast = (ary, fn) => reverse(ary) |. getBy(fn) |. getExn;
  let findLastIndex = (ary, fn) =>
    mapWithIndex(ary, (i, elm) => (i, elm))
    |. reverse
    |. getBy(((_i, elm)) => fn(elm))
    |. getExn
    |. fst;
  let head = headExn;
  let indexOfAll = (ary, elm) =>
    mapWithIndex(ary, (i, e) => (i, e))
    |. keep(((_, e)) => e == elm)
    |. map(fst);
  let initial = ary => reverse(ary) |. tailExn |. reverse;
  let range_ = (s, e, step) => Array.rangeBy(s, e - 1, ~step) |. fromArray;
  let initialize2DArray = (w, h, val_) =>
    range_(0, w, 1) |. map((_) => make(h, val_));
  let initializeArrayWithRange = range_;
  let initializeArrayWithRangeRight = (e, s, step) =>
    range_(s, e, step) |. reverse;
  let initializeArrayWithValues = make;
  let join = (ary, sep) => {
    let [hd, ...tl] = ary;
    List.reduce(tl, hd, (a, b) => a ++ sep ++ b);
  };
  let last = ary => reverse(ary) |. headExn;
  let forEachRight = (ary, fn) => reverse(ary) |. forEach(fn);
  let none = (ary, fn) => ! some(ary, fn);
};
