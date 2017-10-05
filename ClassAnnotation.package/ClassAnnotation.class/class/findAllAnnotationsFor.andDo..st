queries-general
findAllAnnotationsFor: aClass andDo: aBlock
	"It will enumerate all kind of annotations which annotate given class"
	self registry do: [ :instances | 
		instances select: [ :each | each annotates: aClass ] thenDo: aBlock ].