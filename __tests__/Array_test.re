open Jest;

describe("Array.all", () => {
  open Expect;

  test("should be true", () => {
    expect(Array.all([1, 2, 3], (x) => x > 0))
      |> toBe(true)
  });
  test("should be false", () =>
    expect(Array.all([1, 2, 3], (x) => x > 2))
      |> toBe(false)
  );
});

describe("Array.any", () => {
  open Expect;

  test("should be true", () =>
    expect(Array.any([1, 2, 3], (x) => x > 2))
      |> toBe(true)
  );
  test("should be false", () => {
    expect(Array.any([1, 2, 3], (x) => x < 0))
      |> toBe(false)
  });
});

describe("Array.bifurcate", () => {
  open Expect;

  test("should extract \"foo\"", () =>
    expect(Array.bifurcate(["beep", "boop", "foo", "bar"], [true, true, false, true]))
      |> toEqual((["beep", "boop", "bar"], ["foo"]))
  );
});

describe("Array.bifurcateBy", () => {
  open Expect;

  test("should extract \"foo\"", () =>
    expect(Array.bifurcateBy(["beep", "boop", "foo", "bar"], (str) => String.contains(str, 'b')))
      |> toEqual((["beep", "boop", "bar"], ["foo"]))
  );
});

describe("Array.take", () => {
  open Expect;

  test("should extract \"bar\"", () =>
    expect(Array.take(3, ["beep", "boop", "foo", "bar"]))
      |> toEqual(["beep", "boop", "foo"])
  );

  test("should return full list when i > List.length()", () =>
    expect(Array.take(5, ["beep", "boop", "foo", "bar"]))
      |> toEqual(["beep", "boop", "foo", "bar"])
  );

  test("should return empty list when i <= 0", () =>
    expect(Array.take(-1, ["beep", "boop", "foo", "bar"]))
      |> toEqual([])
  );
});
