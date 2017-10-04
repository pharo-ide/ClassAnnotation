testing
isSimilarTo: anotherClassAnnotation

	^self class = anotherClassAnnotation class 
		and: [annotatedClass = anotherClassAnnotation annotatedClass
			and: [activeContext = anotherClassAnnotation activeContext]]