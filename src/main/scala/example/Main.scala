object Main extends App {

	sealed trait Entity {
		def species: String
		def sym: Char

		// Seems like there should be a better way to do 
		// the following
		def isAnimal: Boolean = 
			this match {
				case Animal(_, _) => true
				case _ => false
			}

		def isPlant: Boolean = 
			this match {
				case Plant(_, _, _) => true
				case _ => false
			}

		def isTerrain: Boolean = 
			this match {
				case Terrain(_, _) => true
				case _ => false
			}
	}
	final case class Terrain(species: String, sym: Char) extends Entity
	final case class Animal(species: String, sym: Char) extends Entity
	final case class Plant(species: String, sym: Char, size: Int) extends Entity

	final case class Species(name: String, sym: Char)

	type Map = Array[Array[Entity]]

	val w = 30
	val h = 15

	def clearScreen = print("\u001b[2J")

	def draw(gmap: Map) = {
		clearScreen
		println(
			gmap.transpose.map(
				row => row.map(e => e.sym).mkString
			).mkString("\n")
		)
		println()
	}

	def update(gmap: Map): Map = {
		val newMap = gmap.clone
		newMap
	}

	def initialize: Map = {
		val gmap : Map = Array.fill(w, h)(Terrain("dirt", '.'))

		gmap(5)(10) = Animal("herbivore", 'H')
		gmap(25)(5) = Animal("herbivore", 'H')

		// Check if we can generate a plant in this spot and randomly decide if 
		// a plant should be generated here
		val initPlantSize = 5
		val plantChance = .05
		def genPlant(e: Entity) : Entity = {
			val r = scala.util.Random.nextFloat
			if (e.isTerrain && r < plantChance) 
				Plant("bush", 'b', initPlantSize)
			else 
				e
		}

		gmap.map(row => row.map(e => genPlant(e)))
	}

	// Put this stuff in more reasonable places!
	val getRegex = """get (\d+) (\d+)""".r
	val nextRegex = """n (\d+)""".r
	val helpText = """
		help:   display help text
		n: advance to next step in sim
		n <x>: advance x steps in sim (showing 1 step per second)
		r: run simulation until stopped
		q: quit simulation
		t: print table of info of all non-terrain entities
		get <x> <y>: display info about the entity at (x,y)
	"""

	def gameLoop(gmap: Map) : Unit = {
		// Shows info about the object at (x,y) or an error
		// if that's out of bounds.
		def getInfo (x: Int, y: Int) : String = {
			if (x >= w || y >= h || x < 0 || y < 0)
				s"Location ($x, $y) out of bounds"
			else
				// IMPLEMENT
				s"At ($x, $y):"
		}

		// Read command input from the user
		def getInput : Unit = {
			val input = scala.io.StdIn.readLine("cmd> ")
			input match {
				case "help" => println("Enter n(ext) to advance or q(uit) to stop"); getInput
				case nextRegex(x) => gameLoop(gmap, x.toInt)
				case "n" => gameLoop(update(gmap))
				case "r" => println("Not yet implemented"); getInput // IMPLEMENT
				case "t" => println("Not yet implemented"); getInput // IMPLEMENT					
				case getRegex(x,y) => println(getInfo(x.toInt,y.toInt)); getInput // sketchy toInt?
				case "q" => ()
				case _ => println("Input not recognized; use help for a list of commands."); getInput
			}
		}

		draw(gmap)
		getInput
	}

	// If n isn't a positive number, will just show one screen
	def gameLoop(gmap: Map, n: Int) : Unit = {
		Thread.sleep(1000)
		val newMap = update(gmap)
		if (n > 1) gameLoop(newMap, n-1)
		else gameLoop(newMap)
	}

	gameLoop(initialize)
}