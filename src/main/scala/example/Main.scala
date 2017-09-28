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

	val herbivore = Species("herbivore", 'H')
	val bush = Species("bush", 'b')
	val dirt = Species("dirt", '.')

	type Map = Array[Array[Entity]]

	val w = 30
	val h = 15

	def draw(gmap: Map) = {
		println(
			gmap.transpose.map(
				row => row.map(e => e.species.sym).mkString
			).mkString("\n")
		)
	}

	def update(gmap: Map): Map = {
		gmap
	}

	def initialize: Map = {
		val gmap : Map = Array.fill(w, h)(Terrain(dirt))

		gmap(5)(10) = Animal(herbivore)
		gmap(25)(5) = Animal(herbivore)

		// Check if we can generate a plant in this spot and randomly decide if 
		// a plant should be generated here
		def genPlant(i: Int, j: Int) : Boolean = {
			val plantChance = .05
			gmap(i)(j).isTerrain && scala.util.Random.nextFloat < plantChance
		}

		val plants = 
			for (i <- 0 until w; j <- 0 until h)
				if (genPlant(i, j)) gmap(i)(j) = Plant(bush)

		gmap
	}

	draw(initialize)
}