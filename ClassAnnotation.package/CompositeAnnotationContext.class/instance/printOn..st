printing
printOn: aStream
	super printOn: aStream.
	
	aStream nextPut: $(.
	parts do: [ :each | 
		each printOn: aStream.
		aStream nextPutAll: ', ' ].
	aStream skip: -1; nextPut: $)