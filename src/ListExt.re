open Belt;

module List = {
  include Belt.List;
  let all = (fn, ary) => every(ary, fn);
  let any = (fn, ary) => some(ary, fn);
  let bifurcate = (filter: list(bool), ary) => {
    let (x, y) = zip(ary, filter) |. partition(snd);
    (map(x, fst), map(y, fst));
  };
  let bifurcateBy = (fn, ary) => partition(ary, fn);
  let takeLast = (i, ary) => reverse(ary) |. take(i) |. Option.getWithDefault([]) |. reverse;
  let takeWhile = {
    let rec takeWhile_ = (ary, func, acc) =>
      switch (ary) {
      | [head, ...tail] when func(head) =>
        takeWhile_(tail, func, acc @ [head])
      | _ => acc
      };
    (ary, func) => takeWhile_(ary, func, []);
  };
  let takeLastWhile = (func, ary) =>
    reverse(ary) |. takeWhile(func) |. reverse;
  let chunk = {
    let rec chunk_ = (i, ary, acc) => {
      let len = length(ary);
      if (i <= 0) {
        [];
      } else if (len <= i) {
        acc @ [ary];
      } else {
        chunk_(i, takeLast(len - i, ary), acc @ [take(ary, i) |. Option.getWithDefault([])]);
      };
    };
    (i, ary) => chunk_(i, ary, []);
  };
  let countOccurrences = {
    let rec countOccurrences_ = (value, arr, acc) =>
      switch (arr) {
      | [] => acc
      | [a] when a == value => acc + 1
      | [_] => acc
      | [a, ...b] when a == value => countOccurrences_(value, b, acc + 1)
      | [_, ...b] => countOccurrences_(value, b, acc)
      };
    (value, arr) => countOccurrences_(value, arr, 0);
  };
  let drop = (i, ary) => takeLast(length(ary) - i, ary);
  let dropRight = (i, ary) => take(ary, length(ary) - i) |. Option.getWithDefault([]);
  let dropRightWhile = takeWhile;
  let everyNth = {
    let rec everyNth_ = (nth, ary, acc, index) => {
      let skip = index mod nth != 0;
      switch (ary) {
      | [] => acc
      | [hd, ...tl] =>
        (skip ? acc : acc @ [hd]) |. everyNth_(nth, tl, _, index + 1)
      };
    };
    (nth, ary) => everyNth_(nth, ary, [], 1);
  };
  let getExn = a =>
    switch (a) {
    | Some(v) => v
    | None => raise(Not_found)
    };
  let findLast = (fn, ary) => reverse(ary) |. getBy(fn) |. getExn;
  let findLastIndex = (fn, ary) =>
    mapWithIndex(ary, (i, elm) => (i, elm))
    |. reverse
    |. getBy(((_i, elm)) => fn(elm))
    |. getExn
    |. fst;
  let head = headExn;
  let filteri = (fn, ary) =>
    mapWithIndex(ary, (i, a) => (i, a)) |. keep(fn);
  let indexOfAll = (elm, ary) =>
    mapWithIndex(ary, (i, e) => (i, e))
    |. keep(((_, e)) => e == elm)
    |. map(fst);
  let initial = ary => reverse(ary) |. tailExn |. reverse;
  let range = (s, e, step) => {
    let rec range_ = (s, e, step, acc) =>
      if (s >= e) {
        acc;
      } else {
        range_(s + step, e, step, concat(acc, [s]));
      };
    range_(s, e, step, []);
  };
  let initialize2DArray = (w, h, val_) =>
    range(0, w, 1)
    |. map((_) => range(val_, h + val_, 1))
    |. map(elm => map(elm, (_) => val_));
  let initializeArrayWithRange = range;
  let initializeArrayWithRangeRight = (e, s, step) => range(s, e, step) |. reverse;
};
