open Jest;

open Expect;

describe("Array.all", () => {
  test("should be true", () =>
    expect(Array.all(x => x > 0, [1, 2, 3])) |> toBe(true)
  );
  test("should be false", () =>
    expect(Array.all(x => x > 2, [1, 2, 3])) |> toBe(false)
  );
});

describe("Array.any", () => {
  test("should be true", () =>
    expect(Array.any(x => x > 2, [1, 2, 3])) |> toBe(true)
  );
  test("should be false", () =>
    expect(Array.any(x => x < 0, [1, 2, 3])) |> toBe(false)
  );
});

describe("Array.bifurcate", () =>
  test("should extract \"foo\"", () =>
    expect(
      Array.bifurcate(
        [true, true, false, true],
        ["beep", "boop", "foo", "bar"]
      )
    )
    |> toEqual((["beep", "boop", "bar"], ["foo"]))
  )
);

describe("Array.bifurcateBy", () =>
  test("should extract \"foo\"", () =>
    expect(
      Array.bifurcateBy(
        str => String.contains(str, 'b'),
        ["beep", "boop", "foo", "bar"]
      )
    )
    |> toEqual((["beep", "boop", "bar"], ["foo"]))
  )
);

describe("Array.take", () => {
  test("should extract \"bar\"", () =>
    expect(Array.take(3, ["beep", "boop", "foo", "bar"]))
    |> toEqual(["beep", "boop", "foo"])
  );
  test("should return full list when i > List.length()", () =>
    expect(Array.take(5, ["beep", "boop", "foo", "bar"]))
    |> toEqual(["beep", "boop", "foo", "bar"])
  );
  test("should return empty list when i <= 0", () =>
    expect(Array.take(-1, ["beep", "boop", "foo", "bar"])) |> toEqual([])
  );
});

describe("Array.takeLast", () => {
  test("should extract \"beep\"", () =>
    expect(Array.takeLast(3, ["beep", "boop", "foo", "bar"]))
    |> toEqual(["boop", "foo", "bar"])
  );
  test("should return full list when i > List.length()", () =>
    expect(Array.takeLast(5, ["beep", "boop", "foo", "bar"]))
    |> toEqual(["beep", "boop", "foo", "bar"])
  );
  test("should return empty list when i <= 0", () =>
    expect(Array.takeLast(-1, ["beep", "boop", "foo", "bar"])) |> toEqual([])
  );
});

describe("Array.takeWhile", () => {
  test("should extract [3, 1]", () =>
    expect(Array.takeWhile((i) => i < 3, [1, 2, 3, 1]))
    |> toEqual([1, 2])
  );
});

describe("Array.takeLastWhile", () => {
  test("should extract [1, 2, 3]", () =>
    expect(Array.takeLastWhile((i) => i < 3, [1, 2, 3, 1]))
    |> toEqual([1])
  );
});

describe("Array.chunk", () => {
  test("should return 2 elements list", () =>
    expect(Array.chunk(2, ["beep", "boop", "foo", "bar"]))
    |> toEqual([["beep", "boop"], ["foo", "bar"]])
  );
  test("should return rest elements", () =>
    expect(Array.chunk(3, ["beep", "boop", "foo", "bar"]))
    |> toEqual([["beep", "boop", "foo"], ["bar"]])
  );
  test("should return empty list when size <= 0", () =>
    expect(Array.chunk(-1, ["beep", "boop", "foo", "bar"])) |> toEqual([])
  );
});

describe("Array.countOccurrences", () => {
  test("should return 3", () =>
    expect(Array.countOccurrences(1, [1, 1, 2, 1, 2, 3]))
    |> toEqual(3)
  );

  test("should return 0 when there is no value", () =>
    expect(Array.countOccurrences(-1, [1, 1, 2, 1, 2, 3]))
    |> toEqual(0)
  );

  test("should return 0 when arr is empty", () =>
    expect(Array.countOccurrences(1, []))
    |> toEqual(0)
  );
});

describe("Array.drop", () => {
  test("should drop 1", () =>
    expect(Array.drop(1, [1, 2, 3]))
    |> toEqual([2, 3])
  );
  test("should drop 1, 2", () =>
    expect(Array.drop(2, [1, 2, 3]))
    |> toEqual([3])
  );
  test("should return empty list when i > ary.length", () =>
    expect(Array.drop(42, [1, 2, 3]))
    |> toEqual([])
  );
  test("should not drop when i == 0", () => {
    expect(Array.drop(0, [1, 2, 3]))
    |> toEqual([1, 2, 3])
  });
  test("should not drop when i < 0", () => {
    expect(Array.drop(-10, [1, 2, 3]))
    |> toEqual([1, 2, 3])
  });
});

describe("Array.dropRight", () => {
  test("should drop 3", () =>
    expect(Array.dropRight(1, [1, 2, 3]))
    |> toEqual([1, 2])
  );
  test("should drop 2, 3", () =>
    expect(Array.dropRight(2, [1, 2, 3]))
    |> toEqual([1])
  );
  test("should return empty list when i > ary.length", () =>
    expect(Array.dropRight(42, [1, 2, 3]))
    |> toEqual([])
  );
  test("should not drop when i == 0", () => {
    expect(Array.dropRight(0, [1, 2, 3]))
    |> toEqual([1, 2, 3])
  });
  test("should not drop when i < 0", () => {
    expect(Array.dropRight(-10, [1, 2, 3]))
    |> toEqual([1, 2, 3])
  });
});

describe("Array.dropRightWhile", () => {
  test("should drop [3, 4]", () => {
    expect(Array.dropRightWhile(n => n < 3, [1, 2, 3, 4]))
    |> toEqual([1, 2,])
  });
});

describe("Array.everyNth", () => {
  test("should skip 1, 3, 5", () => {
    expect(Array.everyNth(2, [1, 2, 3, 4, 5, 6]))
    |> toEqual([2, 4, 6])
  });
  test("should skip 1, 2, 4, 5", () => {
    expect(Array.everyNth(3, [1, 2, 3, 4, 5, 6]))
    |> toEqual([3, 6])
  });
});

describe("Array.findLast", () => {
  test("should skip 4", () => {
    expect(Array.findLast((n) => n mod 2 == 1, [1, 2, 3, 4]))
    |> toEqual(3)
  });
});

describe("Array.findLastIndex", () => {
  test("should skip 4", () => {
    expect(Array.findLastIndex((n) => n mod 2 == 1, [1, 2, 3, 4]))
    |> toEqual(2)
  });
});

describe("Array.head", () => {
  test("should get 1", () => {
    expect(Array.head([1, 2, 3, 4]))
    |> toEqual(1)
  });
});

describe("Array.indexOfAll", () => {
  test("should get indices", () => {
    expect(Array.indexOfAll(1, [1, 2, 3, 1, 2, 3]))
    |> toEqual([0, 3])
  });

  test("should get empty list", () => {
    expect(Array.indexOfAll(4, [1, 2, 3, 1, 2, 3]))
    |> toEqual([])
  });
});

describe("Array.initial", () => {
  test("should drop last element", () => {
    expect(Array.initial([1, 2, 3]))
    |> toEqual([1, 2])
  });
});

describe("Array.initialize2DArray", () => {
  test("should generate 2 x 2 List filled with 0", () => {
    expect(Array.initialize2DArray(2, 2, 0))
    |> toEqual([[0, 0], [0, 0]])
  });
});
