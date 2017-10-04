for tests
for: aContext withAnnotatedClass: aClass
	"This method is created to be used in tests. 
	Normally annotatedClass is initialized by system during annotation registry building"
	
	^(self for: aContext) 
		annotatedClass: aClass