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
