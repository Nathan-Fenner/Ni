"use strict"

var BIGINT_BASE = 10

function BigInt(n) {
	this.data = [];
	this.sign = (n >= 0) ? 1 : -1
	n = n / this.sign
	while (n > 0) {
		this.data.push(n % BIGINT_BASE)
		n = Math.floor(n / BIGINT_BASE)
	}
}

// private, mutating
BigInt.prototype.clean = function() {
	while (this.data.length > 0 && !this.data[this.data.length-1]) {
		this.data.pop()
	}
	if (this.data[this.data.length-1] < 0) {
		for (var i = 0; i < this.data.length; i++) {
			this.data[i] *= -1
		}
		this.sign *= -1
	}
	return this
}

// private, mutating
BigInt.prototype.pad = function(that) {
	while (this.data.length < that.data.length) {
		this.data.push(0)
	}
	return this
}

BigInt.prototype.toFloat = function() {
	var s = 0
	var m = 1
	for (var i = 0; i < this.data.length; i++) {
		s += this.data[i] * m
		m *= BIGINT_BASE
	}
	return s * this.sign
}

BigInt.prototype.negate = function() {
	var b = new BigInt(0)
	b.data = this.data
	b.sign = -this.sign
	return b
}

BigInt.prototype.add = function(that) {
	this.pad(that)
	that.pad(this)
	var out = []
	var carry = 0
	for (var i = 0; i < this.data.length; i++) {
		var n = this.data[i] * this.sign + that.data[i] * that.sign + carry
		carry = 0
		while (n >= BIGINT_BASE) {
			n -= BIGINT_BASE
			carry++
		}
		while (n < 0) {
			n += BIGINT_BASE
			carry--
		}
		out[i] = n
	}
	if (carry) {
		out.push(carry)
	}
	var b = new BigInt(0)
	b.sign = 1
	b.data = out
	if (out[out.length-1] < 0) {
		for (var i = 0; i < out.length; i++) {
			out[i] = -out[i]
		}
		b.sign = -1
	}
	return b.clean()
}

BigInt.prototype.multiply = function(that) {
	var out = []
	while (out.length < this.data.length + that.data.length + 1) {
		out.push(0)
	}
	var b = new BigInt(0)
	b.data = out
	b.sign = this.sign * that.sign
	for (var i = 0; i < this.data.length; i++) {
		for (var j = 0; j < that.data.length; j++) {
			var n = i + j
			out[n] += this.data[i] * that.data[j]
			while (out[n] >= BIGINT_BASE) {
				out[n] -= BIGINT_BASE
				out[n+1]++
			}
		}
	}
	return b.clean()
}

BigInt.prototype.equalTo = function(that) {
	this.clean()
	that.clean()
	if (this.sign == that.sign && this.data.length == that.data.length) {
		for (var i = 0; i < this.data.length; i++) {
			if (this.data[i] != that.data[i]) {
				return false
			}
		}
		return true
	}
	return false
}

BigInt.prototype.subtract = function(that) {
	return this.add(that.negate())
}

BigInt.prototype.abs = function() {
	var b = new BigInt(0)
	b.sign = 1
	b.data = this.data
	return b
}

BigInt.prototype.lessThan = function(that) {
	this.clean()
	that.clean()
	if (this.sign != that.sign) {
		return this.sign < that.sign
	}
	if (this.sign > 0) {
		// Both positive
		if (this.data.length == that.data.length) {
			// Same digit count
			for (var i = this.data.length - 1; i >= 0; i--) {
				if (this.data[i] < that.data[i]) {
					return true
				}
				if (this.data[i] > that.data[i]) {
					return false
				}
			}
			return false // equal
		} else {
			return this.data.length < that.data.length
		}
	}
	if (this.sign < 0) {
		// Both negative
		if (this.data.length == that.data.length) {
			for (var i = this.data.length - 1; i >= 0; i--) {
				if (this.data[i] < that.data[i]) {
					return false
				}
				if (this.data[i] > that.data[i]) {
					return true
				}
			}
			return false // equal
		} else {
			return this.data.length > that.data.length
		}
	}
}

BigInt.prototype.divide = function(that) {
	if (this.abs().lessThan(that.abs())) {
		return new BigInt(0)
	}
	var d = []
	while (d.length < this.data.length) {
		d.push(0)
	}
	var a = this.abs()
	var b = that.abs()
	var m = new BigInt(0)
	m.data = d
	m.sign = 1
	for (var i = d.length - 1; i >= 0; i--) {
		d[i] = BIGINT_BASE
		while (d[i] > 0 && a.lessThan(m.multiply(b))) {
			d[i]--
		}
	}
	var b = new BigInt(0)
	b.data = d
	b.sign = this.sign * that.sign
	return b.clean()
}

BigInt.prototype.mod = function(that) {
	var about = this.divide(that).multiply(that)
	var k = this.subtract(about);
	// Sign of... left?
	return k.clean()
	// Always positive answer:
	/*if (k.lessThan(new BigInt(0))) {
		return k.add(that)
	} else {
		return k
	}*/
}

BigInt.prototype.toString = function() {
	if (BIGINT_BASE == 10) {
		var s = "";
		for (var i = this.data.length - 1; i >= 0; i--) {
			s += this.data[i]
		}
		if (this.sign < 0) {
			return "-" + s
		} else {
			return s
		}
	} else {
		return "new BigInt(" + this.toFloat() + ")"
	}
}
