class Group {
  #contents;

  constructor() {
    this.#contents = [];
  }

  has(x) {
    return this.#contents.includes(x); // Checks if the element is in the array
  }

  add(x) {
    if (!this.has(x)) {
      this.#contents.push(x); // Add element to the array if not already present
    }
  }

  delete(x) {
    this.#contents = this.#contents.filter((e) => e !== x); // Filter out the element from the array
  }

  static from(it) {
    let result = new Group();
    for (let x of it) {
      result.add(x); // Add each element from the iterable to the group
    }
    return result;
  }
}

function main() {
  let group = Group.from([10, 20]);
  console.log(group.has(10)); // → true
  console.log(group.has(30)); // → false
  group.add(10);
  group.delete(10);
  console.log(group.has(10)); // → false

  // Additional test cases:

  // Test case 1: Add new elements
  group.add(30);
  console.log(group.has(30)); // → true

  // Test case 2: Check if duplicate elements are not added
  group.add(30);
  console.log(group.has(30)); // → true

  // Test case 3: Delete non-existing element
  group.delete(40); // Should not throw an error
  console.log(group.has(40)); // → false

  // Test case 4: Add and delete multiple elements
  group.add(40);
  group.add(50);
  group.delete(40);
  console.log(group.has(40)); // → false
  console.log(group.has(50)); // → true

  // Test case 5: Empty group
  let emptyGroup = new Group();
  console.log(emptyGroup.has(1)); // → false

  // Test case 6: Adding strings
  let stringGroup = Group.from(["apple", "banana", "cherry"]);
  console.log(stringGroup.has("apple")); // → true
  console.log(stringGroup.has("orange")); // → false
  stringGroup.add("orange");
  console.log(stringGroup.has("orange")); // → true
  stringGroup.delete("banana");
  console.log(stringGroup.has("banana")); // → false

  // Test case 7: Complex types (objects)
  let objectGroup = Group.from([{ a: 1 }, { b: 2 }]);
  console.log(objectGroup.has({ a: 1 })); // → false (because objects are compared by reference, not by value)

  // Test case 8: Large group
  let largeGroup = Group.from(Array.from({ length: 10000 }, (_, i) => i + 1));
  console.log(largeGroup.has(5000)); // → true
  console.log(largeGroup.has(10001)); // → false

  // Test case 9: Mixed types
  let mixedGroup = Group.from([1, "two", { three: 3 }]);
  console.log(mixedGroup.has(1)); // → true
  console.log(mixedGroup.has("two")); // → true
  console.log(mixedGroup.has({ three: 3 })); // → false (object comparison by reference)
}

if (require.main === module) {
  main();
}

module.exports = Group;
