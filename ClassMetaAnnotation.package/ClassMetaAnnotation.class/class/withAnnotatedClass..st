instance creation
withAnnotatedClass: aClass
	"This method is created to be used in tests. 
	Normally annotatedClass is initialized by system during annotation registry building"
	
	^self new 
		annotatedClass: aClass