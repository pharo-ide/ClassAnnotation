printing
printDeclarationOn: aStream

	aStream 
		nextPut: $(;
		nextPutAll: annotatedClass name;
		nextPutAll: ' class>>';
		nextPutAll: declarationSelector;
		nextPut: $)