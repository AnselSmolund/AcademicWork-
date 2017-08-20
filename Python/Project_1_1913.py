#    Ansel Smolund   
from collections import defaultdict
class Random:
    def __init__(self,seed):
        self.number = seed
        
    def next(self,range):
        random = (16807*self.number)%(2**31-1)
        self.number = random
      
        return random%range
        
    def choose(self,objects):
        return objects[self.next(len(objects))] 

 
class Nonce:
    def __init__(self,seed):
        self.first = []
        self.follow = defaultdict(list)
        self.num = seed
        self.random = Random(self.num)
        
    def add(self,word):
        self.w = word
        if(self.w == ""):
           return ""
        self.first+=self.w[0]
        for i in range(0,len(self.w)-1):
            self.follow[self.w[i]].append(self.w[i+1])
        return None

    def make(self,size):
        self.max = size - 1
        if(len(self.first) != 0):
            word = self.random.choose(self.first)
            while(self.max > 0):
            
                if word[-1] in self.follow.keys():
                    InList = self.follow[word[-1]]
                    word = word + self.random.choose(InList)
                else:
                    word = word + self.random.choose(self.first)
                self.max -= 1
            return word
        else:
            raise RuntimeError('No example words have been added') 
        

#spanish        


nw2 = Nonce(2)
nw = Nonce(3)
nw3 = Nonce(99999)
nw.add("vivir")
nw.add("esquirla")
nw.add("almendra")
nw.add("purpura")
nw.add("serenidad")
nw.add("caricia")
nw.add("diafano")
nw.add("cascabel")
nw.add("sangre")
nw.add("piruli")
nw.add("fantasia")
nw.add("aceituna")
nw.add("azar")
nw.add("anoranza")
nw.add("milagro")
nw.add("abecedario")
nw.add("frenesi")
nw.add("derramar")
nw.add("ojala")
nw.add("mandarina")
nw.add("éxtasis")
nw.add("frutilla")
nw.add("hechizo")
nw.add("majestuoso")
nw.add("fascinante")
nw2.add("bisou")
nw2.add("brindille")
nw2.add("chuchoter")
nw2.add("citrouille")
nw2.add("coquelicot")
nw2.add("effleurer")
nw2.add("éphémére")
nw2.add("époustouflant")
nw2.add("epouvantail")
nw2.add("florilége")
nw2.add("folie")
nw2.add("gingermbre")
nw2.add("inoubiable")
nw2.add("libelule")
nw2.add("pamplemousse")
nw2.add("parapluie")
nw2.add("peaufiner")
nw2.add("robinet")
nw2.add("tournesol")
nw3.add("ada")
nw3.add("algol")
nw3.add("postscript")
nw3.add("curl")
nw3.add("bliss")
nw3.add("ceylon")
nw3.add("clojure")
nw3.add("dart")
nw3.add("eiffel")
nw3.add("elephant")
nw3.add("elisp")
nw3.add("falcon")
nw3.add("fortran")
nw3.add("go")
nw3.add("mathematica")
nw3.add("javascript")
nw3.add("intercal")
nw3.add("haskell")
nw3.add("prolog")
nw3.add("python")
nw3.add("ruby")
nw3.add("scheme")
nw3.add("swift")
nw3.add("wolfram")
nw3.add("xenox")
nw3.add("apple")
nw3.add("uber")
nw3.add("tesla")
nw3.add("spacex")
nw3.add("microsoft")
nw3.add("lenovo")
nw.add('ada')  
nw.add('algol')  
nw.add('bliss')  
nw.add('ceylon')  
nw.add('clojure')  
nw.add('curl')  
nw.add('dart')  
nw.add('eiffel')  
nw.add('elephant')  
nw.add('elisp')  
nw.add('falcon')  
nw.add('fortran')  
nw.add('go')  
nw.add('groovy')  
nw.add('haskell')  
nw.add('heron')  
nw.add('intercal')  
nw.add('java')  
nw.add('javascript')  
nw.add('latex')  
nw.add('lisp')  
nw.add('mathematica')  
nw.add('nice')  
nw.add('oak')  
nw.add('occam')  
nw.add('orson')  
nw.add('pascal')  
nw.add('postscript')  
nw.add('prolog')  
nw.add('python')  
nw.add('ruby')  
nw.add('scala')  
nw.add('scheme')  
nw.add('self')  
nw.add('snobol')  
nw.add('swift')  
nw.add('tex')  
nw.add('wolfram')



for i in range(0,10):
    print(nw.make(6))
    print(nw2.make(6))
    print(nw3.make(6))



