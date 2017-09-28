object Main extends App {

	sealed trait Entity {
		def species: Species
		def isTerrain: Boolean = 
			this match {
				case Terrain(s) => true
				case _ => false
			}
	}
	final case class Terrain(species: Species) extends Entity
	final case class Animal(species: Species) extends Entity
	final case class Plant(species: Species) extends Entity

	final case class Species(name: String, sym: Char)

	type Map = Array[Array[Entity]]

	val w = 30
	val h = 15

	def clearScreen = print("\u001b[2J")

	def draw(gmap: Map) = {
		clearScreen
		println(
			gmap.transpose.map(
				row => row.map(e => e.species.sym).mkString
			).mkString("\n")
		)
		println()
	}

	def update(gmap: Map): Map = {
		gmap
	}

	def initialize: Map = {
		val gmap : Map = Array.fill(w, h)(Terrain(Species("dirt", '.')))

		gmap(5)(10) = Animal(Species("herbivore", 'H'))
		gmap(25)(5) = Animal(Species("herbivore", 'H'))

		// Check if we can generate a plant in this spot and randomly decide if 
		// a plant should be generated here
		def genPlant(i: Int, j: Int) : Boolean = {
			val plantChance = .05
			gmap(i)(j).isTerrain && scala.util.Random.nextFloat < plantChance
		}

		val plants = 
			for (i <- 0 until w; j <- 0 until h)
				if (genPlant(i, j)) gmap(i)(j) = Plant(Species("bush", 'b'))

		gmap
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
		get <x> <y>: display info about the entity at (x,y)
	"""

	def gameLoop(gmap: Map) : Unit = {
		// Shows info about the object at (x,y) or an error
		// if that's out of bounds.
		def getInfo (x: Int, y: Int) : String = {
			if (x >= w || y >= h || x < 0 || y < 0)
				s"Location (${x}, ${y}) out of bounds"
			else
				s"At (${x}, ${y}):"
		}

		// Read command input from the user
		def getInput : Unit = {
			val input = scala.io.StdIn.readLine("cmd> ")
			input match {
				case "help" => println("Enter n(ext) to advance or q(uit) to stop"); getInput
				case nextRegex(x) => gameLoop(gmap, x.toInt)
				case "n" => gameLoop(update(gmap))
				case "r" => println("Not yet implemented"); getInput // IMPLEMENT
				case getRegex(x,y) => println(getInfo(x.toInt,y.toInt)); getInput // sketchy toInt?
				case "q" => ()
				case _ => println("Input not recognized; use help for a list of commands."); getInput
			}
		}

		draw(gmap)
		getInput
	}

	def gameLoop(gmap: Map, n: Int) : Unit = {
		assert(n > 0)
		draw(gmap)
		Thread.sleep(1000)
		if (n == 1)
			gameLoop(update(gmap))
		else
			gameLoop(update(gmap), n-1)
	}

	gameLoop(initialize)
}