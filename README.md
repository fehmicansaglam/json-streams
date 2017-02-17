# json-streams

```scala
  val source = Source.fromString {
    """
      |{
      |    "id": 1,
      |    "name": "A green door",
      |    "price": 12.50,
      |    "tags": ["home", "green"]
      |    "inStock": true
      |    "extra": null
      |}
    """.stripMargin
  }

  val reader = new JsonEventReader(source)

  case class Product(id: Option[Long] = None, name: Option[String] = None, price: Option[Double] = None)

  def parseProduct(product: Product): Product = {
    if (reader.hasNext) {
      reader.next() match {
        case JsonKey("id") =>
          val JsonNumber(id) = reader.next()
          parseProduct(product.copy(id = Some(id.longValue())))
        case JsonKey("name") =>
          val JsonString(name) = reader.next()
          parseProduct(product.copy(name = Some(name)))
        case JsonKey("price") =>
          val JsonNumber(price) = reader.next()
          parseProduct(product.copy(price = Some(price.doubleValue())))
        case JsonObjectEnd => product
        case _ => parseProduct(product)
      }
    } else product
  }

  println(parseProduct(Product()))
```
