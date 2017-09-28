accessing
findAllAnnotationsFor: aClass andDo: aBlock
	
	self registry do: [ :instances | 
		instances select: [ :each | each annotates: aClass ] thenDo: aBlock ].