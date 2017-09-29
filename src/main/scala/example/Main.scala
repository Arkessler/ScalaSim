object Main extends App {
	sealed trait Entity {
		def species: String
		def sym: Char
		def loc: (Int,Int)

		// Seems like there should be a better way to do 
		// the following
		def isAnimal: Boolean = 
			this match {
				case Animal(_, _, _) => true
				case _ => false
			}

		def isPlant: Boolean = 
			this match {
				case Plant(_, _, _, _) => true
				case _ => false
			}

		def isTerrain: Boolean = 
			this match {
				case Terrain(_, _, _) => true
				case _ => false
			}
	}
	final case class Terrain(species: String, sym: Char, loc: (Int,Int)) extends Entity
	final case class Animal(species: String, sym: Char, loc: (Int,Int)) extends Entity
	final case class Plant(species: String, sym: Char, size: Int, loc: (Int,Int)) extends Entity

	type Map = Array[Array[(Entity)]]

	// Probably make these non-global at some point
	val w = 30
	val h = 15
	var r = scala.util.Random

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

	def checkBounds(l: (Int,Int)) = {
		val (x, y) = l
		// val (w, h) = (gmap.length, gmap(0).length)
		(x >= 0 && x < w) && (y >= 0 && y < h)
	}

	def update(gmap: Map): Map = {
		val oldPlants : List[Plant] = gmap.flatten.filter(_.isPlant).map(_.asInstanceOf[Plant]).toList
		val oldHerbs : List[Animal] = gmap.flatten.filter(_.isAnimal).map(_.asInstanceOf[Animal]).toList

		// Get adjacent locations that satisfy the valid parameter
		def getValidAdj(currLoc: (Int,Int), valid: (Int,Int) => Boolean) = {
			val (x, y) = currLoc
			val locs = List((x+1, y), (x-1, y), (x, y+1), (x, y-1))
			locs.filter { case (x,y) => valid(x,y) }
		}

		// Plants currently get eaten in a circle by herbivores
		val newPlants = 
			oldPlants.filter( (p: Plant) => {
				getValidAdj(p.loc, (i:Int, j:Int) => {
					checkBounds(i,j) && gmap(i)(j).species == "herbivore"
				}).isEmpty
			})

		val newHerbs =
			oldHerbs.foldLeft(List.empty : List[Animal])((newHerbs : List[Animal], h : Animal) => {

				// Check for adjacent plants (and eat them)
				val plantLocs = getValidAdj(h.loc, (i:Int, j:Int) => {
					 checkBounds(i,j) && gmap(i)(j).isPlant
				})
				if (!plantLocs.isEmpty) h :: newHerbs

				// Randomly move to an empty adjacent location
				val validLocs = getValidAdj(h.loc, (i:Int, j:Int) => {
					val newLocs = newHerbs.map(_.loc)
					checkBounds(i,j) && gmap(i)(j).isTerrain && (!newLocs.contains((i,j)))
				})

				if (!validLocs.isEmpty) {
					val rand = r.nextInt(validLocs.length)
					h.copy(loc = validLocs(rand)) :: newHerbs
				}
				else
					h :: newHerbs

			})

		Array.tabulate(w, h)((x, y) => {
			val dirt = Terrain("dirt", '.', (x, y))
			if (newHerbs.map(_.loc).contains((x, y)))
				newHerbs.find(_.loc == (x,y)).getOrElse(dirt)
			else if (oldPlants.map(_.loc).contains((x,y))) 
				newPlants.find(_.loc == (x,y)).getOrElse(dirt)
			else 
				dirt
		})		
	}

	def initialize: Map = {
		// Check if we can generate a plant in this spot and randomly decide if 
		// a plant should be generated here
		val initPlantSize = 5
		val plantChance = .05
		val herbivoreLocations = List((5,10), (25,5), (3,3), (4,4), (5,5), (20, 8))

		Array.tabulate(w, h)((x, y) =>
			if (herbivoreLocations.contains((x, y)))
				Animal("herbivore", 'H', (x, y))
			else if (r.nextFloat < plantChance) 
				Plant("bush", 'O', initPlantSize, (x, y)) 
			else 
				Terrain("dirt", '.', (x, y))
		)
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
		i: reinitialize map
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
				case "i" => gameLoop(initialize)				
				case getRegex(x,y) => println(getInfo(x.toInt,y.toInt)); getInput // sketchy toInt?
				case "q" => ()
				case _ => println("Input not recognized; use help for a list of commands."); getInput
			}
		}

		draw(gmap)
		getInput
	}

	def waitRestOfSecond(interval: Long) = {
		val waitTime = 1000000000 - interval
		Thread.sleep(waitTime/1000000)
	}

	// If n isn't a positive number, will just show one screen
	def gameLoop(gmap: Map, n: Int) : Unit = {
		val startTime = System.nanoTime()
		
		val newMap = update(gmap)
		draw(newMap)

		val interval = System.nanoTime() - startTime
		waitRestOfSecond(interval)

		if (n > 1) gameLoop(newMap, n-1)
		else gameLoop(newMap)
	}

	gameLoop(initialize)
}