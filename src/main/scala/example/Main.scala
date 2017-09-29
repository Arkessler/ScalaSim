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

	// Probably make some of these non-global at some point
	var r = scala.util.Random
	val maxPlantSize = 5
	val maxPlants = 30

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

	// Note: our gmap implementation is weird, we probably should've just gone with 3
	// lists of the stuff and built the map up from that; currently we had to do some
	// weird stuff to avoid  issues of an herbivore wanting to move into the same spot 
	// that a new plant is being placed or a carnivore wants to move
	def update(gmap: Map): Map = {
		val (w, h) = (gmap.length, gmap(0).length)

		// TODO: Weird w/h reversal stuff
		def checkBounds(l: (Int,Int)) = {
			val (x, y) = l
			(x >= 0 && x < w) && (y >= 0 && y < h)
		}

		// Get adjacent locations that satisfy the valid parameter
		def getConditionalAdj(currLoc: (Int,Int), cond: (Int,Int) => Boolean) = {
			val (x, y) = currLoc
			val locs = List( (x+1, y), (x-1, y), (x, y+1), (x, y-1) )
			locs.filter { case (x,y) => cond(x,y) }
		}

		val oldPlants = gmap.flatten.collect{ case p @ Plant(_,_,_,_) => p }.toList
		val oldHerbs = gmap.flatten.collect{ case h @ Animal("herbivore",_,_) => h }.toList
		val oldCarns = gmap.flatten.collect{ case c @ Animal("carnivore",_,_) => c }.toList

		// Plants currently get eaten "in a circle" by herbivores
		val updatedPlants = 
			oldPlants.map((p: Plant) => {
				val adjHerbs = getConditionalAdj(p.loc, (i:Int, j:Int) => {
					checkBounds(i,j) && gmap(i)(j).species == "herbivore"
				})
				
				val newSize = 
					if (adjHerbs.isEmpty) 
						if (p.size < maxPlantSize) p.size + 1 
						else p.size
					else p.size - adjHerbs.length

				// Decide whether to be lower or uppercase
				if (newSize <= maxPlantSize/2)
					p.copy(size = newSize, sym = p.sym.toLower)
				else 
					p.copy(size = newSize, sym = p.sym.toUpper)
			}).filter(_.size > 0)

		val newPlants = 
			updatedPlants.foldLeft(List.empty : List[Plant])((newPs: List[Plant], p: Plant) => {
				// Decide whether or not to generate a plant
				val totalPlants = oldPlants.length + newPs.length
				val genChance = if (totalPlants == 0) 1 
				                else 1 - totalPlants.toDouble/maxPlants.toDouble
				if (r.nextFloat < genChance) {
					// Possible adj spots for new plants
					val plantLocs = getConditionalAdj(p.loc, (i:Int, j:Int) => {
						val newLocs = newPs.map(_.loc)
						checkBounds(i,j) && gmap(i)(j).isTerrain && !newLocs.contains((i,j))
					})
					if (!plantLocs.isEmpty) {
						val rand = r.nextInt(plantLocs.length)
						val newP = Plant("bush", 'o', 1, plantLocs(rand))
						newP :: newPs
					}
					else newPs
				}
				else newPs
			}) ::: updatedPlants

		val newHerbs =
			oldHerbs.foldLeft(List.empty : List[Animal])((newHs: List[Animal], h: Animal) => {
				// Stay next to adjacent plants
				val plantLocs = getConditionalAdj(h.loc, (i:Int, j:Int) =>
					 checkBounds(i,j) && gmap(i)(j).isPlant
				)
				if (!plantLocs.isEmpty) h :: newHs else {
					// Randomly move to an empty adjacent location
					val validLocs = getConditionalAdj(h.loc, (i:Int, j:Int) => {
						val newLocs = newHs.map(_.loc)
						checkBounds(i,j) && gmap(i)(j).isTerrain &&
							!newLocs.contains((i,j)) && !newPlants.contains((i,j))
					})

					if (!validLocs.isEmpty) {
						val rand = r.nextInt(validLocs.length)
						h.copy(loc = validLocs(rand)) :: newHs
					}
					else h :: newHs
				}
			})

		Array.tabulate(w, h)((x, y) => {
			val dirt = Terrain("dirt", '.', (x, y))
			if (newHerbs.map(_.loc).contains((x, y)))
				newHerbs.find(_.loc == (x,y)).getOrElse(dirt)
			else if (newPlants.map(_.loc).contains((x,y))) 
				newPlants.find(_.loc == (x,y)).getOrElse(dirt)
			else 
				dirt
		})		
	}

	def initialize: Map = {
		// Check if we can generate a plant in this spot and randomly decide if 
		// a plant should be generated here
		val initW = 30
		val initH = 15
		val plantChance = .05
		val carnivoreLocations = List((1,1), (12,12), (8,18))
		val herbivoreLocations = List((5,10), (25,5), (3,3), (4,4), (5,5), (20, 8))

		assert(initW > 0)
		assert(initH > 0)
		Array.tabulate(initW, initH)((x, y) =>
			if (herbivoreLocations.contains((x, y)))
				Animal("herbivore", 'H', (x, y))
			else if (carnivoreLocations.contains(x,y))
				Animal("carnivore", 'C', (x,y))
			else if (r.nextFloat < plantChance) 
				// randomize initial size
				Plant("bush", 'O', maxPlantSize, (x, y)) 
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
			val (w, h) = (gmap.length, gmap(0).length)
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