package week6_Collections

object Example4_ForQueries {

  case class Book(title: String, authors: List[String])

  val books: List[Book] = List(
    Book(title = "book1 cs", authors = List("author11", "author12")),
    Book(title = "book2 maths", authors = List("author21")),
    Book(title = "book3 cs", authors = List("author31", "author32", "author33")),
    Book(title = "book4", authors = List("author11")))
                                                  //> books  : List[week6_Collections.Example4_ForQueries.Book] = List(Book(book1 
                                                  //| cs,List(author11, author12)), Book(book2 maths,List(author21)), Book(book3 c
                                                  //| s,List(author31, author32, author33)), Book(book4,List(author11)))

  // First query
  for (b <- books; a <- b.authors if a == "author32")
  yield b.title                                   //> res0: List[String] = List(book3 cs)
  
  // Second query
  for (b <- books if (b.title indexOf "cs") >= 0)
  yield b.title                                   //> res1: List[String] = List(book1 cs, book3 cs)

  // Third query - with duplicates
  for {
    b1 <- books
    b2 <- books
    if b1 != b2
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1                                      //> res2: List[String] = List(author11, author11)
 
  // Third query - with distinct
  ( for {
    b1 <- books
    b2 <- books
    if b1 != b2
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
    } yield a1
  ).distinct                                      //> res3: List[String] = List(author11)
 
  // First query again, but using higher order functions instead of `for` expressions
  
  // Original query
  for (b <- books; a <- b.authors if a == "author32")
  yield b.title                                   //> res4: List[String] = List(book3 cs)
  
  // (3) for (x <- e1; y <- e2; s) yield e3
  // (3) e1.flatMap(x => for (y <- e2; s) yield e3)
  books flatMap (b =>
    for(a <- b.authors if a == "author32") yield b.title)
                                                  //> res5: List[String] = List(book3 cs)
 
  // (2) for (x <- e1 if f; s) yield e2
  // (2) for (x <- e1.withFilter(x => f); s) yield e2
  books flatMap (b =>
    for(a <- b.authors.withFilter(a => a == "author32")) yield b.title)
                                                  //> res6: List[String] = List(book3 cs)
    
  // (1) for (x <- e1) yield e2
  // (1) e1.map(x => e2)
  books flatMap (b =>
    b.authors.withFilter(a => a == "author32").map(a => b.title))
                                                  //> res7: List[String] = List(book3 cs)
 
}