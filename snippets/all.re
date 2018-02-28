let all = (ary, fn) => List.for_all(fn, ary);

let result1 = all([1, 2, 3], (x) => x > 2);
let result2 = all([1, 2, 3], (x) => x > 0);

Printf.printf("result1 = %B\n", result1);
Printf.printf("result2 = %B\n", result2);
