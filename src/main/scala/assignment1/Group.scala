package assignment1

class Group[T]:
  private var contents = List[T]()

  // Method to add an element to the group if it is not already present
  def add(x: T): Unit = {
    if !has(x) then
      contents = x :: contents // Prepend the element to the list
  }

  // Method to delete an element from the group
  def delete(x: T): Unit = {
    contents = contents.filter(_ != x) // Create a new list without the element
  }

  // Method to check if an element exists in the group
  def has(x: T): Boolean = {
    contents.contains(x) // Check if the element is in the list
  }

object Group:
  // Factory method to create a group from an iterable object
  def from[T](xs: Iterable[T]): Group[T] = {
    val result = new Group[T] // Create an empty group
    for x <- xs do
      result.add(x) // Add each element from the iterable to the group
    result
  }

@main def test() = {
  // Test case 1: Basic functionality with integers
  println("Test case 1: Basic functionality with integers")
  val group1 = Group.from(List(10, 20))
  println(group1.has(10))  // → true
  println(group1.has(30))  // → false

  // Test case 2: Adding and deleting an element
  println("\nTest case 2: Adding and deleting an element")
  group1.add(30)
  println(group1.has(30))  // → true
  group1.delete(10)
  println(group1.has(10))  // → false

  // Test case 3: Adding duplicate elements
  println("\nTest case 3: Adding duplicate elements")
  group1.add(20)           // 20 is already in the group, should not add again
  println(group1.has(20))  // → true

  // Test case 4: Empty group
  println("\nTest case 4: Empty group")
  val group2 = Group.from(List[Int]()) // Empty group
  println(group2.has(1))   // → false
  group2.add(1)
  println(group2.has(1))   // → true

  // Test case 5: Working with strings
  println("\nTest case 5: Working with strings")
  val group3 = Group.from(List("apple", "banana", "cherry"))
  println(group3.has("apple"))   // → true
  println(group3.has("orange"))  // → false
  group3.add("orange")
  println(group3.has("orange"))  // → true
  group3.delete("banana")
  println(group3.has("banana"))  // → false

  // Test case 6: Handling multiple additions and deletions
  println("\nTest case 6: Handling multiple additions and deletions")
  val group4 = Group.from(List(1, 2, 3, 4, 5))
  println(group4.has(5))   // → true
  group4.delete(5)
  println(group4.has(5))   // → false
  group4.add(5)
  group4.add(6)
  group4.add(7)
  println(group4.has(6))   // → true
  println(group4.has(7))   // → true

  // Test case 7: Adding complex types (tuples)
  println("\nTest case 7: Adding complex types (tuples)")
  val group5 = Group.from(List((1, "one"), (2, "two")))
  println(group5.has((1, "one"))) // → true
  group5.delete((1, "one"))
  println(group5.has((1, "one"))) // → false

  // Test case 8: Adding the same object multiple times (objects should be unique)
  println("\nTest case 8: Adding the same object multiple times")
  val group6 = Group.from(List(1, 2, 3))
  group6.add(1)  // Adding 1 again (should not add duplicate)
  println(group6.has(1))  // → true
  println(group6.has(4))  // → false

  // Test case 9: Mixed types (Int and String)
  println("\nTest case 9: Mixed types (Int and String)")
  val group8 = Group.from(List(1, 2, "three", "four"))
  println(group8.has(1))        // → true
  println(group8.has("three"))  // → true
  println(group8.has(5))        // → false
  println(group8.has("five"))   // → false

  // Test case 10: Large collection
  // Takes really long to compute (maybe a few minutes)
  println("\nTest case 10: Large collection")
  val largeList = (1 to 1000000).toList
  val group7 = Group.from(largeList)
  println(group7.has(500000)) // → true
  println(group7.has(1000001)) // → false
}