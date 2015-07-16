
class Counter {
	field value : Int = 0
	func ~ increment {
		this.value++
	}
	func ~ add (amount : Int) {
		this.value += amount
	}
	func ~ reset! {
		print this.value!
		this.value = 0
	}
}

func main {
	var counter = new Counter
	var i = 0
	while i < 200 {
		i *= 2
		counter ~ add i
	}
	counter ~ reset!
}

