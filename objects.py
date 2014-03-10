class Test:
	def __init__(this):
		print "Calls init() on " + str(this)
		this.integer = 0

	def tests(this,integer):
		print "Calls tests(" + str(integer) + ") on " + str(this)
		this.integer=integer

test = Test()
test.tests(2)
print test.integer
