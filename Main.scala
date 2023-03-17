import scala.collection.immutable.ArraySeq
import scala.io.Source

/**
 * Main app containg program loop
 */
object Main extends App {

  println("Starting application")

  val status = run()

  println("\nExiting application")
  println(s"Final status: ${status.message}")

  /**
   * Read action from Stdin and execute it
   * Exit if action is 'exit' or if an error occured (status > 0)
   * DO NOT MODIFY THIS FUNCTION
   */
  def run(canvas: Canvas = Canvas()): Status = {
    println("\n======\nCanvas:")
    canvas.display

    print("\nAction: ")

    val action = scala.io.StdIn.readLine()

    val (newCanvas, status) = execute(ArraySeq.unsafeWrapArray(action.split(' ')), canvas)

    if (status.error) {
      println(s"ERROR: ${status.message}")
    }

    if (status.exit) {
      status 
    } else {
      run(newCanvas)  
    }
  }

  /**
   * Execute various actions depending on an action command and optionnaly a Canvas
   */
  def execute(action: Seq[String], canvas: Canvas): (Canvas, Status) = {
    val execution: (Seq[String], Canvas) => (Canvas, Status) = action.head match {
      case "exit" => Canvas.exit
      case "dummy" => Canvas.dummy
      case "dummy2" => Canvas.dummy2
      case "new_canvas" => Canvas.createCanvas
      case "load_image" => Canvas.loadImage
      case "update_pixel" => Canvas.updatePixel
      case "draw" => Canvas.draw
      case _ => Canvas.default
    }

    execution(action.tail, canvas)
  }
}

/**
 * Define the status of the previous execution
 */
case class Status(
  exit: Boolean = false,
  error: Boolean = false,
  message: String = ""
)

/**
 * A pixel is defined by its coordinates along with its color as a char
 */
case class Pixel(x: Int, y: Int, color: Char = ' ') {
  override def toString(): String = {
    color.toString
  }
}

/**
 * Companion object of Pixel case class
 */
object Pixel {
  /**
   * Create a Pixel from a string "x,y"
   */
  def apply(s: String): Pixel = {
    val coordinates = s.split(",").map(_.trim).map(_.toInt)
    Pixel(coordinates(0), coordinates(1))
  }

  /**
   * Create a Pixel from a string "x,y" and a color 
   */
  def apply(s: String, color: Char): Pixel = {
    val coordinates = s.split(",").map(_.trim).map(_.toInt)
    Pixel(coordinates(0), coordinates(1), color)
  }
}

/**
 * A Canvas is defined by its width and height, and a matrix of Pixel
 */
case class Canvas(width: Int = 0, height: Int = 0, pixels: Vector[Vector[Pixel]] = Vector()) {

  /**
   * Print the canvas in the console
   */
  def display: Unit = {
    if (pixels.size == 0) {
      println("Empty Canvas")
    } else {
      println(s"Size: $width x $height")
      // TODO
    }
  }

  /**
   * Takes a pixel in argument and put it in the canvas
   * in the right position with its color
   */
  def update(pixel: Pixel): Canvas = {
    
    val newPixels = pixels.updated(pixel.y, pixels(pixel.y).updated(pixel.x, pixel))

    this.copy(pixels = newPixels)
  }

  /**
   * Return a Canvas containing all modifications
   */
  def updates(pixels: Seq[Pixel]): Canvas = {
    pixels.foldLeft(this)((f, p) => f.update(p))
  }
  // TODO: Add any useful method
}

/**
 * Companion object for Canvas case class
 */
object Canvas {
  /**
   * Exit execution
   */
  def exit(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = 
    (canvas, Status(exit = true, message="Received exit signal"))

  /**
   * Default execution for unknown action
   */
  def default(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = 
    (canvas, Status(error = true, message = s"Unknown command"))

  /**
   * Create a static Canvas
   */
  def dummy(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = 
    if (arguments.size > 0) 
      (canvas, Status(error = true, message = "dummy action does not excpect arguments"))
    else  {
      val dummyCanvas = Canvas(
        width = 3,
        height = 4,
        pixels = Vector(
          Vector(Pixel(0, 0, '#'), Pixel(1, 0, '.'), Pixel(2, 0, '#')),
          Vector(Pixel(0, 1, '#'), Pixel(1, 1, '.'), Pixel(2, 1, '#')),
          Vector(Pixel(0, 2, '#'), Pixel(1, 2, '.'), Pixel(2, 2, '#')),
          Vector(Pixel(0, 3, '#'), Pixel(1, 3, '.'), Pixel(2, 3, '#'))
        )
      )
      printCanvas(dummyCanvas)

      (dummyCanvas, Status())
    }

  /**
   * Create a static canvas using the Pixel companion object
   */
  def dummy2(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = 
    if (arguments.size > 0) 
      (canvas, Status(error = true, message = "dummy action does not excpect arguments"))
    else  {
      val dummyCanvas = Canvas(
        width = 3,
        height = 1,
        pixels = Vector(
          Vector(Pixel("0,0", '#'), Pixel("1,0"), Pixel("2,0", '#')),
        )
      )
      (dummyCanvas, Status())
      printCanvas(dummyCanvas)

    }

  def createCanvas(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = 
    if (arguments.size < 3) 
      (canvas, Status(error = true, message = s"La commande new_canvas attend 3 arguments. Vous en avez donné ${arguments.size}. Voici un exemple de commande valide: new_canvas 13 13 ."))
   else if (!arguments(0).forall(_.isDigit) || !arguments(1).forall(_.isDigit) || arguments(2).size != 1)
      (canvas, Status(error = true, message = "Il y a une erreur dans le type des arguments. Voici un exemple de commande valide: new_canvas 13 13 ."))
    else  {
      val dummyCanvas = Canvas(
        width = arguments(0).toInt,
        height = arguments(1).toInt,
        pixels = Vector.fill(arguments(1).toInt)(Vector.fill(arguments(0).toInt)(Pixel(0, 0, arguments(2).head)))
      )

      printCanvas(dummyCanvas)
      (dummyCanvas, Status())
    }

  def loadImage(arguments: Seq[String], canvas: Canvas): (Canvas, Status) =
    if (arguments.size < 1)
      (canvas, Status(error = true, message = "La commande load_image attend 1 argument. Vous en avez donné 0"))
    else {
      val fileName = arguments(0)
      try {
        val lines: Vector[String] = Source.fromFile(fileName).getLines().toVector
        val pixels = lines.map { line =>
          line.map(char => Pixel(0, 0, char)).toVector
        }
        val newCanvas = Canvas(pixels(0).size, pixels.size, pixels)
        printCanvas(newCanvas)
        (newCanvas, Status())
      } catch {
        case e: Exception => (canvas, Status(error = true, message = s"Erreur de chargement de l'image: $e. Vérifiez que le fichier existe et que vous avez les droits d'accès."))
      }
    }

  def updatePixel(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = {
    if (arguments.size < 3) {
      (canvas, Status(error = true, message = s"La commande update_pixel attend 3 arguments. Vous en avez donné ${arguments.size}. Voici un exemple de commande valide: update_pixel 0 0 ."))
    }
    if (!arguments(0).forall(_.isDigit) || !arguments(1).forall(_.isDigit) || arguments(2).size != 1) {
      (canvas, Status(error = true, message = "Il y a une erreur de type dans les arguments: Voici un exemple de commande valide: update_pixel 0 0 ."))
    }

    val newCanvas = canvas.update(Pixel(arguments(0).toInt, arguments(1).toInt, arguments(2).head))
    printCanvas(newCanvas)
    
    (newCanvas, Status())
  }

  def draw(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = 
    if (arguments.size < 2) 
      (canvas, Status(error = true, message = s"La commande draw attend au moins 2 arguments. Vous en avez donné ${arguments.size}."))
    else {
      arguments(0) match {
        case "line" => drawLine(arguments, canvas)
        case "rectangle" => drawRectangle(arguments, canvas)
        case "triangle" => drawTriangle(arguments, canvas)
        case "polygon" => drawPolygon(arguments, canvas)
        case "fill" => fill(arguments, canvas)
        case _ => (canvas, Status(error = true, message = s"La commande ${arguments(0)} n'existe pas. Essayez une de ces commandes line, rectangle, triangle, polygon ou fill."))
      }
    }
  
  def printCanvas(canvas: Canvas): Unit = 
    canvas.pixels.foreach { row =>
      row.foreach { pixel =>
      print(pixel.color)
      }
      println()
    }

  def drawLine(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = {
    if (arguments.size < 4) {
      return (canvas, Status(error = true, message = s"La commande line attend 3 arguments. Or vous en avez donné ${arguments.size}."))
    }

    if (!arguments(1).forall(_.isDigit) || !arguments(2).forall(_.isDigit) || !arguments(3).forall(_.isDigit)) {
      return (canvas, Status(error = true, message = s"La commande line attend des arguments de ces types: line x1,y1 x2,y2 ."))
    }

    val pixel1 = Pixel(arguments(1))
    val pixel2 = Pixel(arguments(2))
    val color = arguments(3).charAt(0)

    val (x1, y1, x2, y2) = (pixel1.x, pixel1.y, pixel2.x, pixel2.y)

    if (x1 < 0 || x1 >= canvas.width || y1 < 0 || y1 >= canvas.height || x2 < 0 || x2 >= canvas.width || y2 < 0 || y2 >= canvas.height) {
      return (canvas, Status(error = true, message = s"Vous avez essayé de dessiner une ligne en dehors du canvas."))
    }

    val (dx, dy) = (math.abs(x2 - x1), math.abs(y2 - y1))

    var x = x1
    var y = y1
    var newCanvas = canvas

    if (dx > dy) {
      val sx = if (x1 < x2) 1 else -1
      val sy = if (y1 < y2) 1 else -1
      var D = 2 * dy - dx

      for (i <- 0 until dx) {
        newCanvas = newCanvas.update(Pixel(x, y, color))
        if (D > 0) {
          y += sy
          D -= 2 * dx
        }
        D += 2 * dy
        x += sx
      }
    } else {
      val sx = if (x1 < x2) 1 else -1
      val sy = if (y1 < y2) 1 else -1
      var D = 2 * dx - dy

      for (i <- 0 until dy) {
        newCanvas = newCanvas.update(Pixel(x, y, color))
        if (D > 0) {
          x += sx
          D -= 2 * dy
        }
        D += 2 * dx
        y += sy
      }
    }
    
    printCanvas(newCanvas)
    (newCanvas, Status())
  }


  def drawRectangle(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = {
    if (arguments.size < 4) {
      return (canvas, Status(error = true, message = s"La commande rectangle attend 3 arguments. Vous en avez donné ${arguments.size}."))
    }

    if (!arguments(1).forall(_.isDigit) || !arguments(2).forall(_.isDigit) || !arguments(3).forall(_.isDigit)) {
      return (canvas, Status(error = true, message = s"La commande rectangle attend des arguments de ces types: rectangle x1,y1 x2,y2 ."))
    }
  
    val pixel1 = Pixel(arguments(1))
    val pixel2 = Pixel(arguments(2))
    val color = arguments(3).charAt(0)

    // dessiner les 4 lignes
    val (x1, y1, x2, y2) = (math.min(pixel1.x, pixel2.x), math.min(pixel1.y, pixel2.y), math.max(pixel1.x, pixel2.x), math.max(pixel1.y, pixel2.y))

    var newCanvas = canvas
    newCanvas = drawLine(Seq("line", s"$x1,$y1", s"$x2,$y1", s"$color"), newCanvas)._1
    newCanvas = drawLine(Seq("line", s"$x1,$y2", s"$x2,$y2", s"$color"), newCanvas)._1
    newCanvas = drawLine(Seq("line", s"$x1,$y1", s"$x1,$y2", s"$color"), newCanvas)._1
    newCanvas = drawLine(Seq("line", s"$x2,$y1", s"$x2,$y2", s"$color"), newCanvas)._1
    (newCanvas, Status())
  }

  def drawTriangle(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = {
    if (arguments.size < 4) {
      return (canvas, Status(error = true, message = s"La commande triangle attend 3 arguments. Vous en avez donné ${arguments.size}."))
    }

    if (!arguments(1).forall(_.isDigit) || !arguments(2).forall(_.isDigit) || !arguments(3).forall(_.isDigit)) {
      return (canvas, Status(error = true, message = s"La commande triangle attend des arguments de ces types: triangle x1,y1 x2,y2 x3,y3 ."))
    }

    val pixel1 = Pixel(arguments(1))
    val pixel2 = Pixel(arguments(2))
    val pixel3 = Pixel(arguments(3))
    val color = arguments(4).charAt(0)

    var newCanvas = canvas
    newCanvas = drawLine(Seq("line", s"${pixel1.x},${pixel1.y}", s"${pixel2.x},${pixel2.y}", s"$color"), newCanvas)._1
    newCanvas = drawLine(Seq("line", s"${pixel2.x},${pixel2.y}", s"${pixel3.x},${pixel3.y}", s"$color"), newCanvas)._1
    newCanvas = drawLine(Seq("line", s"${pixel3.x},${pixel3.y}", s"${pixel1.x},${pixel1.y}", s"$color"), newCanvas)._1
    (newCanvas, Status())
  }

  def drawPolygon(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = {
    if (arguments.size < 3) {
      return (canvas, Status(error = true, message = "La commande polygon attend au moins 2 arguments."))
    }

    if (!arguments.forall(_.forall(_.isDigit))) {
      return (canvas, Status(error = true, message = "La commande polygon attend des arguments de ces types: polygon x1,y1 x2,y2 x3,y3 xn,yn ."))
    }

    val color = arguments(arguments.size - 1).charAt(0)
    var newCanvas = canvas

    for (i <- 1 until arguments.size - 2) {
      val (newCanvas1, _) = drawLine(Seq("line", arguments(i), arguments(i+1), color.toString), newCanvas)
      newCanvas = newCanvas1
    }
    val (newCanvas2, _) = drawLine(Seq("line", arguments(arguments.size - 2), arguments(1), color.toString), newCanvas)
    (newCanvas2, Status())
  }

  def fill(arguments: Seq[String], canvas: Canvas): (Canvas, Status) = {
    if (arguments.size < 3) {
      return (canvas, Status(error = true, message = s"La commande fill attend 2 arguments. Vous en avez donné ${arguments.size}."))
    }

    if (!arguments(1).forall(_.isDigit)) {
      return (canvas, Status(error = true, message = s"La commande fill attend des arguments de ces types: fill x,y ."))
    }

    val pixel = Pixel(arguments(1))

    if (pixel.x < 0 || pixel.x >= canvas.width || pixel.y < 0 || pixel.y >= canvas.height) {
      return (canvas, Status(error = true, message = s"Vous avez donné des coordonnées en dehors du canvas."))
    }

    val color = arguments(2).charAt(0)
    val targetColor = canvas.pixels(pixel.y)(pixel.x).color
    val updatedPixels = fillRecursive(pixel.x, pixel.y, targetColor, color, canvas)
    printCanvas(updatedPixels)
    (canvas, Status())
  }

  def fillRecursive(x: Int, y: Int, targetColor: Char, color: Char, canvas: Canvas): Canvas = {
    if (x < 0 || x >= canvas.width || y < 0 || y >= canvas.height || canvas.pixels(y)(x).color != targetColor) {
      canvas
    } else {
      Seq((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)).foldLeft(canvas.update(Pixel(x, y, color))) { (canvas, coords) =>
        val (xx, yy) = coords
        fillRecursive(xx, yy, targetColor, color, canvas)
      }
    }
  }
  
}
