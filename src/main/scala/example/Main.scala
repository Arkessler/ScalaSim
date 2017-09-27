object Main extends App {
	type Map = Array[Array[Char]]

	val w = 30
	val h = 15

	def drawMap(gmap: Map) = {
		println(
			gmap.map(row => row.mkString).mkString("\n")
		)
	}

	def updateMap(gmap: Map): Map = {
		gmap
	}

	def initialize: Map = {
		val gmap = Array.fill(h, w)('.')
		gmap
	}

	drawMap(initialize)
}