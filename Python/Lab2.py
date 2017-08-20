def main(args):
	class Sieve:
		def __init__(self,max):
			if max < 0:
				raise RuntimeError
			else:
				self.numbers = [True] * max
				self.numbers[0] = self.numbers[1] = False
		
		
		def findPrimes(self):
			
			for i in range(2,len(self.numbers)):
				if self.numbers[i]:
					self.numbers.append(i)
					for n in xrange(i*i,(len(self.numbers)),i):
						self.numbers[n] = False
			return self.numbers
			
		def howMany(self):
			count = 0
			for i in range(2,100):
				if self.numbers[i]:
					count += 1
			return count
		
		def toList(self):
			nums = []
			for i in range(2,100):
				if self.numbers[i]:
					nums = nums + [int(i)]
				
			return nums 
				
		
		
		
		
	S = Sieve(100)
	print(S.howMany())
	S.findPrimes()
	
	print(S.howMany())
	print(S.toList())
			
	

if __name__ == '__main__':
    import sys
    sys.exit(main(sys.argv))
